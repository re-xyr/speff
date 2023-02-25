{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
module Sp.Internal.Monad
  ( Eff
  , Env
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
  ) where

import           Control.Monad          (ap, liftM)
import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import qualified Control.Monad.Catch    as Catch
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.IORef             (IORef)
import           Data.Kind              (Type)
#ifdef SPEFF_NATIVE_DELCONT
import           Sp.Internal.Ctl.Native
#else
import           Sp.Internal.Ctl.Monad
#endif
import qualified Sp.Internal.Env        as Rec
import           Sp.Internal.Env        (Rec, (:>))
import           System.IO.Unsafe       (unsafePerformIO)

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
data HandleTag (tag :: Type) (es :: [Effect]) (r :: Type) = HandleTag (Env es) !(Marker r)

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
unsafeState x0 f = Eff \es -> promptState x0 \ref -> unEff (f ref) es
{-# INLINE unsafeState #-}

-- | Convert an effect handler into an internal representation with respect to a certain effect context and prompt
-- frame.
toInternalHandler :: ∀ e es r. Marker r -> Env es -> Handler e es r -> InternalHandler e
toInternalHandler mark es hdl = InternalHandler \e -> alter Rec.pad $ hdl (HandleTag @() es mark) e

-- | Do a trivial transformation over the effect context.
alter :: (Env es' -> Env es) -> Eff es a -> Eff es' a
alter f = \(Eff m) -> Eff \es -> m $! f es

-- | General effect handling. Introduce a prompt frame, convert the supplied handler to an internal one wrt that
-- frame, and then supply the internal handler to the given function to let it add that to the effect context.
handle :: (InternalHandler e -> Env es' -> Env es) -> Handler e es' a -> Eff es a -> Eff es' a
handle f = \hdl (Eff m) -> Eff \es -> prompt \mark -> m $! f (toInternalHandler mark es hdl) es

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
abort (HandleTag es mark) (Eff m) = Eff \_ -> raise mark $ m es
{-# INLINE abort #-}

-- | Capture and gain control of the resumption. The resumption cannot escape the scope of the controlling function.
control
  :: Localized tag :> esSend
  => HandleTag tag es r
  -> (∀ tag'. (Eff esSend a -> Eff (Localized tag' : es) r) -> Eff (Localized tag' : es) r)
  -> Eff esSend a
control (HandleTag es mark) f =
  Eff \esSend -> yield mark \cont -> unEff (f \(Eff x) -> Eff \_ -> cont $ x esSend) $! Rec.pad es
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

instance IOE :> es => MonadThrow (Eff es) where
  throwM x = Eff \_ -> Catch.throwM x
  {-# INLINE throwM #-}

instance IOE :> es => MonadCatch (Eff es) where
  catch (Eff m) h = Eff \es -> Catch.catch (m es) \ex -> unEff (h ex) es
  {-# INLINE catch #-}

-- | Unwrap an 'Eff' monad with 'IO' computations.
runIOE :: Eff '[IOE] a -> IO a
runIOE m = runCtl $ unEff m (Rec.pad Rec.empty)
{-# INLINE runIOE #-}
