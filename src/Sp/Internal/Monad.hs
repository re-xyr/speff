{-# LANGUAGE AllowAmbiguousTypes #-}
module Sp.Internal.Monad
  ( Eff
  , Effect
  , Handling
  , Handler
  , unsafeIO
  , unsafeState
  , lift
  , interpret
  , reinterpret
  , interpose
  , reinterpose
  , send
  , Localized
  , embed
  , withUnembed
  , abort
  , control
  , runEff
  , (:>)
  , IOE
  , runIOE
  ) where

import           Control.Monad          (ap, liftM)
import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import qualified Control.Monad.Catch    as Catch
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.IORef             (IORef)
import           Data.Kind              (Type)
import           Sp.Internal.Ctl
import qualified Sp.Internal.Env        as Rec
import           Sp.Internal.Env        (Rec, (:>))
import           Sp.Internal.Util       (DictRep, reflectDict)
import           System.IO.Unsafe       (unsafePerformIO)

-- | The kind of higher-order effects.
type Effect = (Type -> Type) -> Type -> Type

type Env = Rec InternalHandler

-- | The effect monad; it is parameterized by a row of effects available. This monad is implemented with evidence
-- passing and an delimited control monad with support of efficient tail-resumptive (non-capturing) computations and
-- @IO@ embedding.
type role Eff nominal representational
newtype Eff (es :: [Effect]) (a :: Type) = Eff { unEff :: Env es -> Ctl a }

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

-- | The handler context. This allows delimited control and scoped effects.
class Handling (esSend :: [Effect]) (es :: [Effect]) (r :: Type) | esSend -> es r where
  handlingDict :: HandlingDict es r
  handlingDict = error
    "Sp.Eff: nonexistent handling context! Don't attempt to manually define an instance for the 'Handling' typeclass."

data HandlingDict es r = Handling (Env es) !(Marker r)
type instance DictRep (Handling _ es r) = HandlingDict es r

handlingEs :: ∀ esSend es r. Handling esSend es r => Env es
handlingEs = case handlingDict @esSend of
  Handling es _ -> es

handlingMarker :: ∀ esSend es r. Handling esSend es r => Marker r
handlingMarker = case handlingDict @esSend of
  Handling _ mark -> mark

-- | The type of effect handlers.
type Handler e es r = ∀ esSend a. Handling esSend es r => e :> esSend => e (Eff esSend) a -> Eff esSend a

-- This "unsafe" IO function is perfectly safe in the sense that it won't panic or otherwise cause undefined
-- behaviors; it is only unsafe when it is used to embed arbitrary IO actions in any effect environment,
-- therefore breaking effect abstraction.
unsafeIO :: IO a -> Eff es a
unsafeIO m = Eff (const $ liftIO m)
{-# INLINE unsafeIO #-}

unsafeState :: s -> (IORef s -> Eff es a) -> Eff es a
unsafeState x0 f = Eff \es -> promptState x0 \ref -> unEff (f ref) es
{-# INLINE unsafeState #-}

toInternalHandler :: ∀ e es r. Marker r -> Env es -> Handler e es r -> InternalHandler e
toInternalHandler mark es hdl = InternalHandler \(e :: e (Eff esSend) a) ->
  reflectDict @(Handling esSend es r) hdl (Handling es mark) e

alter :: (Env es' -> Env es) -> Eff es a -> Eff es' a
alter f (Eff m) = Eff \es -> m $! f es

handle :: Handler e es' a -> (InternalHandler e -> Env es' -> Env es) -> Eff es a -> Eff es' a
handle hdl f (Eff m) = Eff \es -> prompt \mark -> m $! f (toInternalHandler mark es hdl) es

-- | Handle an effect.
interpret :: Handler e es a -> Eff (e : es) a -> Eff es a
interpret hdl = handle hdl Rec.cons
{-# INLINE interpret #-}

-- | Handle an effect with another newly introduced effect. This allows for effect encapsulation because it does not
-- require placing constraints on the original row.
reinterpret :: Handler e (e' : es) a -> Eff (e : es) a -> Eff (e' : es) a
reinterpret hdl = handle hdl \ih es -> Rec.cons ih $ Rec.tail es
{-# INLINE reinterpret #-}

-- | Handle an effect already in the environment. This is particularly useful in scoped effect operations.
interpose :: e :> es => Handler e es a -> Eff es a -> Eff es a
interpose hdl = handle hdl Rec.update
{-# INLINE interpose #-}

-- | Handle an effect already in the environment with another newly introduced effect.
reinterpose :: e :> es => Handler e (e' : es) a -> Eff es a -> Eff (e' : es) a
reinterpose hdl = handle hdl \ih es -> Rec.update ih $ Rec.tail es
{-# INLINE reinterpose #-}

-- | List a computation to a larger effect environment; it can also be thought as "masking" the outermost effect for
-- the computation.
lift :: Eff es a -> Eff (e : es) a
lift = alter Rec.tail
{-# INLINE lift #-}

-- | Perform an effect operation.
send :: e :> es => e (Eff es) a -> Eff es a
send e = Eff \es -> unEff (runHandler (Rec.index es) e) es
{-# INLINE send #-}

-- | A "localized computaton"; this is parameterized with an existential variable so the computation with this effect
-- cannot escape a certain scope.
data Localized (tag :: k) :: Effect

-- | Perform an operation from the handle-site.
embed :: ∀ esSend es r a. Handling esSend es r => Eff es a -> Eff esSend a
embed (Eff m) = Eff \_ -> m $ handlingEs @esSend
{-# INLINE embed #-}

-- | Perform an operation from the handle-site, while being able to convert an operation from the perform-site to the
-- handle-site.
withUnembed
  :: ∀ esSend es r a
  .  Handling esSend es r
  => (∀ tag. (Eff esSend a -> Eff (Localized tag : es) a) -> Eff (Localized tag : es) a)
  -> Eff esSend a
withUnembed f = Eff \esSend -> unEff (f \(Eff m) -> Eff \_ -> m esSend) $! Rec.consNull $ handlingEs @esSend
{-# INLINE withUnembed #-}

-- | Abort with a result value.
abort :: ∀ esSend es r a. Handling esSend es r => Eff es r -> Eff esSend a
abort (Eff m) = Eff \_ -> raise (handlingMarker @esSend) $ m $ handlingEs @esSend
{-# INLINE abort #-}

-- | Yield and gain control of the resumption. The resumption cannot escape the scope of the controlling function.
control
  :: ∀ esSend es r a
  .  Handling esSend es r
  => (∀ tag. (Eff esSend a -> Eff (Localized tag : es) r) -> Eff (Localized tag : es) r)
  -> Eff esSend a
control f = Eff \esSend -> yield (handlingMarker @esSend) \cont ->
  unEff (f \(Eff x) -> Eff \_ -> cont $ x esSend) $! Rec.consNull $ handlingEs @esSend
{-# INLINE control #-}

-- | Unpack the 'Eff' monad.
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

-- | Unpack an 'Eff' monad with 'IO' acitons.
runIOE :: Eff '[IOE] a -> IO a
runIOE m = runCtl $ unEff (interpret (\case) m) Rec.empty
{-# INLINE runIOE #-}
