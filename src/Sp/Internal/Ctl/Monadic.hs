-- |
-- Copyright: (c) 2022 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
--
-- Delimited control monad based on the design of Xie et al. This implementation imposes much less cost (albeit still
-- noticeable) on computations not utilizing delimited control than the naive implementation. It also doesn't rely on
-- compiler-supplied primitive operations.
module Sp.Internal.Ctl.Monadic
  ( Marker
  , Ctl
  , freshMarker
  , prompt
  , control
  , abort
  , promptState
  , runCtl
  , dynamicWind
  , mask
  , uninterruptibleMask
  , interruptible
  ) where

import           Control.Exception      (MaskingState (MaskedInterruptible, MaskedUninterruptible, Unmasked),
                                         SomeException, getMaskingState)
import qualified Control.Exception      as Exception
import           Control.Monad          (ap, liftM, (<=<))
import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import qualified Control.Monad.Catch    as Catch
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.IORef             (IORef, readIORef, writeIORef)
import           Data.Kind              (Type)
import           Data.Primitive.PrimVar (PrimVar, fetchAddInt, newPrimVar)
import           Data.Type.Equality     (type (:~:) (Refl))
import           GHC.Exts               (RealWorld, maskAsyncExceptions#, maskUninterruptible#, unmaskAsyncExceptions#)
import           GHC.IO                 (IO (IO))
import           System.IO.Unsafe       (unsafePerformIO)
import           Unsafe.Coerce          (unsafeCoerce)

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
type role Result representational
data Result (a :: Type)
  = Pure a
  -- ^ The computation returned normally.
  | ∀ (r :: Type). Abort !(Marker r) (Ctl r)
  -- ^ The computation replaced itself with another computation.
  | ∀ (r :: Type) (b :: Type). Control !(Marker r) ((Ctl b -> Ctl r) -> Ctl r) (Ctl b -> Ctl a)
  -- ^ The computation captured a resumption and gained control over it. Specifically, this uses @shift0@ semantics.

-- | Extend the captured continuation with a function, if it exists.
extend :: (Ctl a -> Ctl a) -> Result a -> Result a
extend f = \case
  Pure a                -> Pure a
  Abort mark r          -> Abort mark r
  Control mark ctl cont -> Control mark ctl (f . cont)

-- | The delimited control monad, with efficient support of tail-resumptive computations.
type role Ctl representational
newtype Ctl (a :: Type) = Ctl { unCtl :: IO (Result a) }

instance Functor Ctl where
  fmap = liftM

instance Applicative Ctl where
  pure = Ctl . pure . Pure
  (<*>) = ap

instance Monad Ctl where
  (Ctl x) >>= f = Ctl $ x >>= \case
    Pure a                -> unCtl (f a)
    Abort mark r          -> pure $ Abort mark r
    Control mark ctl cont -> pure $ Control mark ctl (f `compose` cont)

-- | This loopbreaker is crucial to the performance of the monad.
compose :: (b -> Ctl c) -> (a -> Ctl b) -> a -> Ctl c
compose = (<=<)
{-# NOINLINE compose #-}

-- | Lift an 'IO' function to a 'Ctl' function. The function must not alter the result.
liftMap, liftMap' :: (IO (Result a) -> IO (Result a)) -> Ctl a -> Ctl a
liftMap f (Ctl m) = Ctl $ extend (liftMap' f) <$> f m
{-# INLINE liftMap #-}
liftMap' = liftMap
{-# NOINLINE liftMap' #-}

-- | Install a prompt frame.
prompt, prompt' :: Marker a -> Ctl a -> Ctl a
prompt !mark (Ctl m) = Ctl $ m >>= \case
  Pure a -> pure $ Pure a
  Abort mark' r -> case eqMarker mark mark' of
    Just Refl -> unCtl r
    Nothing   -> pure $ Abort mark' r
  Control mark' ctl cont -> case eqMarker mark mark' of
    Just Refl -> unCtl $ ctl (prompt' mark . cont)
    Nothing   -> pure $ Control mark' ctl (prompt' mark . cont)
{-# INLINE prompt #-}
prompt' = prompt
{-# NOINLINE prompt' #-}

-- | Capture the resumption up to and including the prompt frame specified by the 'Marker'.
control :: Marker r -> ((Ctl a -> Ctl r) -> Ctl r) -> Ctl a
control !mark f = Ctl $ pure $ Control mark f id

-- | Replace the current computation up to and including the prompt with a new one.
abort :: Marker r -> Ctl r -> Ctl a
abort !mark r = Ctl $ pure $ Abort mark r

-- | Introduce a mutable state that behaves well wrt reentry.
promptState, promptState' :: IORef s -> Ctl r -> Ctl r
promptState !ref (Ctl m) = Ctl $ m >>= \case
  Pure x -> pure $ Pure x
  Abort mark x -> pure $ Abort mark x
  Control mark ctl cont -> do
    s0 <- liftIO (readIORef ref)
    pure $ Control mark ctl \x -> do
      liftIO (writeIORef ref s0)
      promptState' ref (cont x)
{-# INLINE promptState #-}
promptState' = promptState
{-# NOINLINE promptState' #-}

-- | Unwrap the 'Ctl' monad.
runCtl :: Ctl a -> IO a
runCtl (Ctl m) = m >>= \case
  Pure a     -> pure a
  Abort {}   -> error "Sp.Ctl: Unhandled abort operation. Forgot to pair it with a prompt?"
  Control {} -> error "Sp.Ctl: Unhandled control operation. Forgot to pair it with a prompt?"

instance MonadIO Ctl where
  liftIO = Ctl . fmap Pure

instance MonadThrow Ctl where
  throwM = Ctl . Exception.throwIO

-- | Note that although both catching and masking are possible, implementing 'Catch.generalBracket' via them will not
-- be well-behaved wrt reentry; hence 'Ctl' is not 'Catch.MonadMask'.
instance MonadCatch Ctl where
  catch m h = liftMap (Exception.handle (unCtl . h)) m

-- | Install pre- and post-actions that are well-behaved wrt reentry. Specifically, pre- and post-actions are always
-- guaranteed to act in pairs.
dynamicWind, dynamicWind' :: Ctl () -> Ctl () -> Ctl a -> Ctl a
dynamicWind before after (Ctl action) = do
  res <- before >> Ctl do
    res <- Exception.try @SomeException action
    pure $ Pure res
  after >> Ctl case res of
    Left se -> Exception.throwIO se
    Right y -> pure $ extend (dynamicWind' before after) y
{-# INLINE dynamicWind #-}
dynamicWind' = dynamicWind
{-# NOINLINE dynamicWind' #-}

block, unblock, blockUninterruptible :: Ctl a -> Ctl a
block = liftMap \(IO m) -> IO $ maskAsyncExceptions# m
unblock = liftMap \(IO m) -> IO $ unmaskAsyncExceptions# m
blockUninterruptible = liftMap \(IO m) -> IO $ maskUninterruptible# m

-- | Lifted version of 'Exception.mask'.
mask :: ((∀ x. Ctl x -> Ctl x) -> Ctl a) -> Ctl a
mask io = liftIO getMaskingState >>= \case
  Unmasked              -> block $ io unblock
  MaskedInterruptible   -> io block
  MaskedUninterruptible -> io blockUninterruptible

-- | Lifted version of 'Exception.uninterruptibleMask'.
uninterruptibleMask :: ((∀ x. Ctl x -> Ctl x) -> Ctl a) -> Ctl a
uninterruptibleMask io = liftIO getMaskingState >>= \case
  Unmasked              -> blockUninterruptible $ io unblock
  MaskedInterruptible   -> blockUninterruptible $ io block
  MaskedUninterruptible -> io blockUninterruptible

-- | Lifted version of 'Exception.interruptible'.
interruptible :: Ctl a -> Ctl a
interruptible io = liftIO getMaskingState >>= \case
  Unmasked              -> io
  MaskedInterruptible   -> unblock io
  MaskedUninterruptible -> io
