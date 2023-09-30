{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
-- |
-- Copyright: (c) 2022 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
--
-- The effect monad, along with handling combinators that enable delimited control and higher-order effects.
module Sp.Internal.Monad
  ( Eff
  , Env
  , InternalHandler
  , Effect
  , HandleTag
  , Handler
  , unsafeIO
  , unsafeState
  , handle
  , alter
  , send
  , Localized
  , embed
  , withUnembed
  , abort
  , control
  , runEff
  , IOE
  , runIOE
  , dynamicWind
  , mask
  , uninterruptibleMask
  , interruptible
  ) where

#ifdef SPEFF_NATIVE_DELCONT
#define CTL_MODULE Sp.Internal.Ctl.Native
#else
#define CTL_MODULE Sp.Internal.Ctl.Monadic
#endif

import           Control.Monad          (ap, liftM)
import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import qualified Control.Monad.Catch    as Catch
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           CTL_MODULE             (Ctl, runCtl)
import qualified CTL_MODULE             as Ctl
import           Data.IORef             (IORef, newIORef)
import           Data.Kind              (Type)
import qualified Sp.Internal.Env        as Rec
import           Sp.Internal.Env        (Rec, (:>))
import           System.IO.Unsafe       (unsafePerformIO)

#undef CTL_MODULE

-- | The kind of higher-order effects, parameterized by (1) the monad in which it was performed, and (2) the result
-- type.
type Effect = (Type -> Type) -> Type -> Type

-- | The concrete representation of an effect context: a record of internal handler representations.
type Env = Rec InternalHandler

-- | The effect monad; it is parameterized by the /effect context/, i.e. a row of effects available. This monad is
-- implemented with evidence passing and a delimited control monad with support of efficient tail-resumptive
-- (non-capturing) computations and @IO@ embedding.
type role Eff nominal representational
newtype Eff (es :: [Effect]) (a :: Type) = Eff { unEff :: Env es -> Ctl a }

-- | The internal representation of a handler of effect @e@. This representation is only valid within the original
-- context in which the effect was introduced.
type role InternalHandler nominal
newtype InternalHandler e = InternalHandler
  { runHandler :: ∀ es a. e :> es => e (Eff es) a -> Eff es a }

instance Functor (Eff es) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Applicative (Eff es) where
  pure x = Eff \_ -> pure x
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (Eff es) where
  Eff m >>= f = Eff \es -> m es >>= \x -> unEff (f x) es
  {-# INLINE (>>=) #-}

-- | The tag associated to a handler that was /introduced/ in context @es@ over an computation with
-- /eventual result type/ @r@. Value of this type enables delimited control and scoped effects.
data HandleTag (tag :: Type) (es :: [Effect]) (r :: Type) = HandleTag (Env es) !(Ctl.Marker r)

-- | A handler of effect @e@ introduced in context @es@ over a computation returning @r@.
type Handler e es r = ∀ tag esSend a. e :> esSend => HandleTag tag es r -> e (Eff esSend) a -> Eff (Localized tag : esSend) a

-- | This "unsafe" @IO@ function is perfectly safe in the sense that it won't panic or otherwise cause undefined
-- behaviors; it is only unsafe when it is used to embed arbitrary @IO@ actions in any effect environment,
-- therefore breaking effect abstraction.
unsafeIO :: IO a -> Eff es a
unsafeIO m = Eff (const $ liftIO m)
{-# INLINE unsafeIO #-}

-- | Introduce a mutable state that is well-behaved with respect to reentry. That is, no branch will observe mutations
-- by any other simultaneous branch. This stops behaving as expected if you pass the 'IORef' out of scope.
unsafeState :: s -> (IORef s -> Eff es a) -> Eff es a
unsafeState x0 f = Eff \es -> do
  ref <- liftIO $ newIORef x0
  Ctl.promptState ref $ unEff (f ref) es
{-# INLINE unsafeState #-}

-- | Convert an effect handler into an internal representation with respect to a certain effect context and prompt
-- frame.
toInternalHandler :: ∀ e es r. Ctl.Marker r -> Env es -> Handler e es r -> InternalHandler e
toInternalHandler mark es hdl = InternalHandler \e -> alter Rec.pad $ hdl (HandleTag @() es mark) e

-- | Do a trivial transformation over the effect context.
alter :: (Env es' -> Env es) -> Eff es a -> Eff es' a
alter f = \(Eff m) -> Eff \es -> m $! f es

-- | General effect handling. Introduce a prompt frame, convert the supplied handler to an internal one wrt that
-- frame, and then supply the internal handler to the given function to let it add that to the effect context.
handle :: (InternalHandler e -> Env es' -> Env es) -> Handler e es' a -> Eff es a -> Eff es' a
handle f = \hdl (Eff m) -> Eff \es -> do
  mark <- Ctl.freshMarker
  Ctl.prompt mark $ m $! f (toInternalHandler mark es hdl) es

-- | Perform an effect operation.
send :: e :> es => e (Eff es) a -> Eff es a
send e = Eff \es -> unEff (runHandler (Rec.index es) e) es
{-# INLINE send #-}

-- | A "localized computaton"; this should be parameterized with an existential variable so the computation with this
-- effect cannot escape a certain scope.
data Localized (tag :: Type) :: Effect

-- | Perform an operation from the handle-site.
embed :: Localized tag :> esSend => HandleTag tag es r -> Eff es a -> Eff esSend a
embed (HandleTag es _) (Eff m) = Eff \_ -> m es
{-# INLINE embed #-}

-- | Perform an operation from the handle-site, while being able to convert an operation from the perform-site to the
-- handle-site.
withUnembed
  :: Localized tag :> esSend
  => HandleTag tag es r
  -> (∀ tag'. (∀ x. Eff esSend x -> Eff (Localized tag' : es) x) -> Eff (Localized tag' : es) a)
  -> Eff esSend a
withUnembed (HandleTag es _) f =
  Eff \esSend -> unEff (f \(Eff m) -> Eff \_ -> m esSend) $! Rec.pad es
{-# INLINE withUnembed #-}

-- | Abort with a result value.
abort :: Localized tag :> esSend => HandleTag tag es r -> Eff es r -> Eff esSend a
abort (HandleTag es mark) (Eff m) = Eff \_ -> Ctl.abort mark $ m es
{-# INLINE abort #-}

-- | Capture and gain control of the resumption. The resumption cannot escape the scope of the controlling function.
control
  :: Localized tag :> esSend
  => HandleTag tag es r
  -> (∀ tag'. (Eff esSend a -> Eff (Localized tag' : es) r) -> Eff (Localized tag' : es) r)
  -> Eff esSend a
control (HandleTag es mark) f =
  Eff \esSend -> Ctl.control mark \cont -> unEff (f \(Eff x) -> Eff \_ -> cont $ x esSend) $! Rec.pad es
{-# INLINE control #-}

-- | Unwrap the 'Eff' monad.
runEff :: Eff '[] a -> a
runEff (Eff m) = unsafePerformIO (runCtl $ m Rec.empty)
{-# INLINE runEff #-}

-- | Ability to embed 'IO' side effects.
data IOE :: Effect

instance IOE :> es => MonadIO (Eff es) where
  liftIO = unsafeIO
  {-# INLINE liftIO #-}

instance MonadThrow (Eff es) where
  throwM x = Eff \_ -> Catch.throwM x
  {-# INLINE throwM #-}

instance IOE :> es => MonadCatch (Eff es) where
  catch (Eff m) h = Eff \es -> Catch.catch (m es) \ex -> unEff (h ex) es
  {-# INLINE catch #-}

-- | Unwrap an 'Eff' monad with 'IO' computations.
runIOE :: Eff '[IOE] a -> IO a
runIOE m = runCtl $ unEff m (Rec.pad Rec.empty)
{-# INLINE runIOE #-}

-- | Attach a pre- and a post-action to a computation. The pre-action runs immediately before the computation, and the
-- post-action runs immediately after the computation exits, whether normally, via an error, or via an exception.
-- Additionally, the post-action runs immediately after any suspension of the enclosed computation and the pre-action
-- runs immediately before any resumption of such suspension.
--
-- Therefore, this function acts like 'Control.Exception.bracket_' when there is no resumption of suspensions involved,
-- except providing no protection against async exceptions. If you want such protection, please manually supply masked
-- actions.
--
-- In all cases, it is guaranteed that pre- and post-actions are always executed in pairs; that is to say, it is
-- impossible to have two calls to the pre-action without a call to the post-action interleaving them, or vice versa.
-- This also means that the pre- and post-action are always executed the same number of times, discounting
-- interruptions caused by async exceptions.
dynamicWind :: Eff es () -> Eff es () -> Eff es a -> Eff es a
dynamicWind (Eff before) (Eff after) (Eff action) =
  Eff \es -> Ctl.dynamicWind (before es) (after es) (action es)
{-# INLINE dynamicWind #-}

-- | Lifted version of 'Control.Exception.mask'.
mask :: IOE :> es => ((∀ x. Eff es x -> Eff es x) -> Eff es a) -> Eff es a
mask f = Eff \es -> Ctl.mask \unmask -> unEff (f \(Eff m) -> Eff \es' -> unmask (m es')) es
{-# INLINE mask #-}

-- | Lifted version of 'Control.Exception.uninterruptibleMask'.
uninterruptibleMask :: IOE :> es => ((∀ x. Eff es x -> Eff es x) -> Eff es a) -> Eff es a
uninterruptibleMask f = Eff \es -> Ctl.uninterruptibleMask \unmask -> unEff (f \(Eff m) -> Eff \es' -> unmask (m es')) es
{-# INLINE uninterruptibleMask #-}

-- | Lifted version of 'Control.Exception.interruptible'.
interruptible :: IOE :> es => Eff es a -> Eff es a
interruptible (Eff m) = Eff \es -> Ctl.interruptible (m es)
{-# INLINE interruptible #-}
