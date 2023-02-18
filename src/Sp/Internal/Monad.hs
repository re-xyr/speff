{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
module Sp.Internal.Monad
  ( Eff
  , Effect
  , Handling
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
import           Sp.Internal.Util       (DictRep, reflectDict)
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

-- | The handler context of a handler that was (1) __introduced__ in context @es@, (2) over an computation with
-- __eventual result type__ @r@, and (3) __handling__ an effect originating from context @esSend@. This typeclass
-- enables delimited control and scoped effects.
class Handling (esSend :: [Effect]) (es :: [Effect]) (r :: Type) | esSend -> es r where
  handlingDict :: HandlingDict es r
  handlingDict = error
    "Sp.Eff: nonexistent handling context! Don't attempt to manually define an instance for the 'Handling' typeclass."

-- | Concrete representation of @Handling esSend es r@. It is revealed that @esSend@ is only a phantom argument used
-- to ensure soundness (as it is the only other context apart from @es@ that is known to be within the handle site
-- context).
data HandlingDict es r = Handling (Env es) !(Marker r)
type instance DictRep (Handling _ es r) = HandlingDict es r

-- | Reify the handle-site context.
withHandling :: ∀ esSend es r a. Handling esSend es r => (HandlingDict es r -> a) -> a
withHandling f = f (handlingDict @esSend)

-- | A handler of effect @e@ introduced in context @es@ over a computation returning @r@.
type Handler e es r = ∀ esSend a. Handling esSend es r => e :> esSend => e (Eff esSend) a -> Eff esSend a

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
toInternalHandler mark es hdl = InternalHandler \(e :: e (Eff esSend) a) ->
  reflectDict @(Handling esSend es r) hdl (Handling es mark) e

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
data Localized (tag :: k) :: Effect

-- | Perform an operation from the handle-site.
embed :: ∀ esSend es r a. Handling esSend es r => Eff es a -> Eff esSend a
embed (Eff m) = withHandling @esSend \(Handling es _) -> Eff \_ -> m es
{-# INLINE embed #-}

-- | Perform an operation from the handle-site, while being able to convert an operation from the perform-site to the
-- handle-site.
withUnembed
  :: ∀ esSend es r a
  .  Handling esSend es r
  => (∀ tag. (Eff esSend a -> Eff (Localized tag : es) a) -> Eff (Localized tag : es) a)
  -> Eff esSend a
withUnembed f = withHandling @esSend \(Handling es _) ->
  Eff \esSend -> unEff (f \(Eff m) -> Eff \_ -> m esSend) $! Rec.consNull es
{-# INLINE withUnembed #-}

-- | Abort with a result value.
abort :: ∀ esSend es r a. Handling esSend es r => Eff es r -> Eff esSend a
abort (Eff m) = withHandling @esSend \(Handling es mark) -> Eff \_ -> raise mark $ m es
{-# INLINE abort #-}

-- | Capture and gain control of the resumption. The resumption cannot escape the scope of the controlling function.
control
  :: ∀ esSend es r a
  .  Handling esSend es r
  => (∀ tag. (Eff esSend a -> Eff (Localized tag : es) r) -> Eff (Localized tag : es) r)
  -> Eff esSend a
control f = withHandling @esSend \(Handling es mark) ->
  Eff \esSend -> yield mark \cont -> unEff (f \(Eff x) -> Eff \_ -> cont $ x esSend) $! Rec.consNull es
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
runIOE m = runCtl $ unEff m (Rec.consNull Rec.empty)
{-# INLINE runIOE #-}
