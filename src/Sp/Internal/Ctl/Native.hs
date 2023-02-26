{-# LANGUAGE CPP              #-}
{-# LANGUAGE UnboxedTuples    #-}
{-# LANGUAGE UnliftedNewtypes #-}
-- |
-- Copyright: (c) 2023 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
--
-- Delimited control monad based on the GHC primops introduced by Alexis King. This implementation is zero cost for
-- computations not utilizing delimited control. On the other hand, frequent capture in this monad is slower than the
-- monadic implementation.
module Sp.Internal.Ctl.Native
  ( Marker
  , Ctl
  , freshMarker
  , prompt
  , yield
  , raise
  , promptState
  , runCtl
  , dynamicWind
  , mask
  , mask_
  , uninterruptibleMask
  , uninterruptibleMask_
  , interruptible
  ) where

import           Control.Exception        (Exception (fromException), SomeException)
import qualified Control.Exception        as Exception
import           Control.Monad.Catch      (MonadCatch (catch), MonadThrow)
import           Control.Monad.Catch.Pure (MonadThrow (throwM))
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Data.IORef               (IORef, readIORef, writeIORef)
import           Data.Kind                (Type)
import           Data.Type.Equality       ((:~:) (Refl))
import           GHC.Exts                 (Any, RealWorld)
#if __GLASGOW_HASKELL__ >= 906
import           GHC.Exts                 (PromptTag#, control0#, newPromptTag#, prompt#)
#else
import           GHC.Exts                 (ByteArray#, RuntimeRep, State#, TYPE)
#endif
import           Data.Primitive.PrimVar   (PrimVar, fetchAddInt, newPrimVar)
import           GHC.IO                   (IO (IO), unIO)
import           System.IO.Unsafe         (unsafePerformIO)
import           Unsafe.Coerce            (unsafeCoerce)

-- Stub definitions intended for developing with GHC < 9.6, which do not have the proper primops
#if __GLASGOW_HASKELL__ < 906
newtype PromptTag# (a :: Type) = PromptTag# ByteArray#

newPromptTag# :: State# RealWorld -> (# State# RealWorld, PromptTag# a #)
newPromptTag# _ = error "newPromptTag#"

prompt#
  :: PromptTag# a
  -> (State# RealWorld -> (# State# RealWorld, a #))
  -> State# RealWorld -> (# State# RealWorld, a #)
prompt# _ _ = error "prompt#"

control0#
  :: ∀ (a :: Type) (r :: RuntimeRep) (b :: TYPE r)
  .  PromptTag# a
  -> (((State# RealWorld -> (# State# RealWorld, b #))
      -> State# RealWorld -> (# State# RealWorld, a #))
    -> State# RealWorld -> (# State# RealWorld, a #))
  -> State# RealWorld -> (# State# RealWorld, b #)
control0# _ _ _ = error "control0#"
#endif

data PromptTag (a :: Type) = PromptTag (PromptTag# a)

-- | We do not utilize the 'PromptTag#' mechanism built into the primops, as they do not support any kind of unwinding
-- handlers to be installed in the middle of the prompt frame and the control call.
thePromptTag :: PromptTag a
thePromptTag = unsafePerformIO $ IO
  \s0 -> case newPromptTag# s0 of
    (# s1, tag #) -> (# s1, PromptTag tag #)
{-# NOINLINE thePromptTag #-}

-- | Install a generic prompt frame without a specific tag.
promptIO :: IO a -> IO a
promptIO (IO m) = case thePromptTag of
  PromptTag tag -> IO (prompt# tag m)

-- | Unwind the stack with the continuation until the first prompt frame created by 'promptIO'.
control0IO :: (∀ r. (IO a -> IO r) -> IO r) -> IO a
control0IO f = case thePromptTag of
  PromptTag tag -> IO $ control0# tag \cont -> unIO $ f \io -> IO (cont $ unIO io)

-- | The source from which we construct unique 'Marker's.
uniqueSource :: PrimVar RealWorld Int
uniqueSource = unsafePerformIO (newPrimVar 0)
{-# NOINLINE uniqueSource #-}

-- | Create a fresh 'Marker'.
freshMarker :: ∀ a. Ctl (Marker a)
freshMarker = liftIO $ Marker <$> fetchAddInt uniqueSource 1

-- | A @'Marker' a@ marks a prompt frame over a computation returning @a@.
type role Marker representational
newtype Marker (a :: Type) = Marker Int

-- | Check the equality of two markers, and if so provide a proof that the type parameters are equal. This does not
-- warrant a @TestEquality@ instance because it requires decidable equality over the type parameters.
eqMarker :: Marker a -> Marker b -> Maybe (a :~: b)
eqMarker (Marker l) (Marker r) =
  if l == r then Just (unsafeCoerce Refl) else Nothing

-- | Intermediate result of a `Ctl` computation.
data Result a
  = Finished a
  | Raised SomeException
  | Unwound (Unwind a)

-- | Unwinding of a 'Ctl' computation.
data Unwind a
  = ∀ (r :: Type) (b :: Type).
    Capture !(Marker r) ((Ctl b -> Ctl r) -> Ctl r) (Ctl b -> Ctl a)
  | ∀ (r :: Type).
    Abort !(Marker r) (Ctl r)

-- | Unwindings are thrown to each prompt frame so that they can manipulate them. The absense of type parameters (hence
-- 'Any') allows us to avoid @Typeable@.
newtype UnwindException = UnwindException (Unwind Any)
  deriving anyclass Exception

instance Show UnwindException where
  show _ = "Escaped unwinding"

-- | The delimited control monad, with efficient support of tail-resumptive computations.
type role Ctl representational
newtype Ctl (a :: Type) = Ctl { runCtl :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO) via IO

-- | Install an general handler.
handle' :: Ctl a -> (Result a -> Ctl b) -> Ctl b
handle' (Ctl m) f = Ctl $ runCtl . f =<< Exception.catch (Finished <$> promptIO m)
  \se -> pure case fromException se of
    Just (UnwindException y) -> Unwound $ unsafeCoerce y
    Nothing                  -> Raised se

-- | Install a handler that only modifies unwindings.
handle :: Ctl a -> (Unwind a -> Ctl a) -> Ctl a
handle (Ctl m) f = Ctl $ Exception.catch (promptIO m)
  \(UnwindException y) -> runCtl $ f $ unsafeCoerce y

-- | Unwind to the nearest handler.
unwind :: Unwind r -> Ctl a
unwind x = Ctl $ Exception.throwIO $ UnwindException $ unsafeCoerce $ x
{-# INLINE unwind #-}

-- | Unwind-with-current-continuation. Useful when the unwind is a capture.
unwindCC :: (∀ r. (Ctl a -> Ctl r) -> Unwind r) -> Ctl a
unwindCC f = Ctl $ control0IO \cont -> runCtl $ unwind $ f (Ctl . cont . runCtl)
{-# INLINE unwindCC #-}

-- | Prompt/reset with a specific marker. This is unsafe.
prompt :: Marker a -> Ctl a -> Ctl a
prompt !mark m = handle m \case
  Abort mark' r -> case eqMarker mark mark' of
    Just Refl -> r
    Nothing   -> raise mark' r
  Capture mark' ctl cont -> case eqMarker mark mark' of
    Just Refl -> ctl (prompt mark . cont)
    Nothing   -> unwindCC \cont' -> Capture mark' ctl (cont' . prompt mark . cont)

-- | Take over control of the continuation up to the prompt frame specified by 'Marker'.
yield :: Marker r -> ((Ctl a -> Ctl r) -> Ctl r) -> Ctl a
yield !mark ctl = unwindCC \cont -> Capture mark ctl cont

-- | Aborts the computation with a value to the prompt frame specified by 'Marker'.
raise :: Marker r -> Ctl r -> Ctl a
raise !mark r = unwind $ Abort mark r

-- | Set up backtracking on a specific state variable. This is unsafe.
promptState :: IORef s -> Ctl a -> Ctl a
promptState !ref m = handle m \case
  Abort mark r -> raise mark r
  Capture mark ctl cont -> do
    s0 <- liftIO $ readIORef ref
    unwindCC \cont' -> Capture mark ctl \x -> cont' do
      liftIO $ writeIORef ref s0
      promptState ref (cont x)

instance MonadThrow Ctl where
  throwM = Ctl . Exception.throwIO

instance MonadCatch Ctl where
  catch (Ctl m) h = Ctl $ Exception.catch m \se ->
    -- Stop users from catching unwindings by catching 'SomeException'
    case fromException @UnwindException se of
      Just u -> Exception.throwIO u
      Nothing -> case fromException se of
        Just e  -> runCtl $ h e
        Nothing -> Exception.throwIO se

-- | Attach pre- and post-actions that are well-behaved in the presence of captures. The downside is that it doesn't
-- support passing the pre-action's result to the main action.
dynamicWind :: Ctl () -> Ctl () -> Ctl a -> Ctl a
dynamicWind before after action =
  before >> handle' action \v -> after >> case v of
    Finished a -> pure a
    Raised se -> throwM se
    Unwound y -> case y of
      Abort mark r -> raise mark r
      Capture mark ctl cont -> unwindCC \cont' ->
        Capture mark ctl (cont' . dynamicWind before after . cont)

-- | Lifted version of 'Exception.mask'.
mask :: ((∀ x. Ctl x -> Ctl x) -> Ctl a) -> Ctl a
mask io = Ctl $ Exception.mask \unmask -> runCtl $ io (Ctl . unmask . runCtl)

-- | Lifted version of 'Exception.mask_'.
mask_ :: Ctl a -> Ctl a
mask_ io = Ctl $ Exception.mask_ $ runCtl io

-- | Lifted version of 'Exception.uninterruptibleMask'.
uninterruptibleMask :: ((∀ x. Ctl x -> Ctl x) -> Ctl a) -> Ctl a
uninterruptibleMask io = Ctl $ Exception.uninterruptibleMask \unmask -> runCtl $ io (Ctl . unmask . runCtl)

-- | Lifted version of 'Exception.uninterruptibleMask_'.
uninterruptibleMask_ :: Ctl a -> Ctl a
uninterruptibleMask_ io = Ctl $ Exception.uninterruptibleMask_ $ runCtl io

-- | Lifted version of 'Exception.interruptible'.
interruptible :: Ctl a -> Ctl a
interruptible io = Ctl $ Exception.interruptible $ runCtl io
