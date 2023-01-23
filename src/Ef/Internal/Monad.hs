{-# LANGUAGE AllowAmbiguousTypes #-}
module Ef.Internal.Monad where

import           Control.Monad          (ap, liftM)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Atomics.Counter   (AtomicCounter, incrCounter, newCounter)
import           Data.Kind              (Type)
import           Data.Type.Equality     (type (:~:) (Refl))
import           Ef.Internal.Env        (Rec, (:>))
import qualified Ef.Internal.Env        as Rec
import           Ef.Internal.Util       (DictRep, axiom, reflectDict)
import           System.IO.Unsafe       (unsafePerformIO)

--- Marker ---

uniqueSource :: AtomicCounter
uniqueSource = unsafePerformIO (newCounter 0)
{-# NOINLINE uniqueSource #-}

type role Marker nominal representational
newtype Marker (es :: [Effect]) (a :: Type) = Marker Int

eqMarker :: Marker es a -> Marker es' a' -> Maybe (Marker es a :~: Marker es' a')
eqMarker (Marker x) (Marker y) =
  if x == y then Just axiom else Nothing

freshMarker :: ∀ es a es'. Eff es' (Marker es a)
freshMarker = unsafeIO $ Marker <$> incrCounter 1 uniqueSource

--- Effect monad ---

type Effect = (Type -> Type) -> Type -> Type

type Env = Rec InternalHandler

type role InternalHandler nominal
data InternalHandler (e :: Effect) =
  forall (esHandle :: [Effect]). InternalHandler
  { handlerEnv :: Env esHandle
  , runHandler :: ∀ esSend a. e :> esSend => e (Eff esSend) a -> Eff esSend a
  }

type role Result nominal representational
data Result (es :: [Effect]) (a :: Type)
  = Pure a
  | ∀ (esTarget :: [Effect]) (r :: Type).
    Abort !(Marker esTarget r) (Eff esTarget r)
  | ∀ (esSend :: [Effect]) (esTarget :: [Effect]) (b :: Type) (r :: Type).
    Capture !(Marker esTarget r) ((Eff esSend b -> Eff esTarget r) -> Eff esTarget r) (Eff esSend b -> Eff es a)

instance Functor (Result es) where
  fmap f = \case
    Pure a                -> Pure (f a)
    Abort mark r          -> Abort mark r
    Capture mark ctl cont -> Capture mark ctl (fmap f . cont)

type role Ctl nominal representational
newtype Ctl (es :: [Effect]) (a :: Type) = Ctl { unCtl :: IO (Result es a) }

instance Functor (Ctl es) where
  fmap = liftM

instance Applicative (Ctl es) where
  pure = Ctl . pure . Pure
  (<*>) = ap

instance Monad (Ctl es) where
  (Ctl x) >>= f = Ctl $ x >>= \case
    Pure a                -> unCtl (f a)
    Abort mark r          -> pure $ Abort mark r
    Capture mark ctl cont -> pure $ Capture mark ctl \v -> Eff \es -> unEff (cont v) es >>= f

instance MonadIO (Ctl es) where
  liftIO m = Ctl $ Pure <$> m

runCtl :: Ctl es a -> IO a
runCtl (Ctl m) = m >>= \case
  Pure a     -> pure a
  Abort {}   -> error "Sp.Ctl: Unhandled abort operation. Forgot to pair it with a prompt?"
  Capture {} -> error "Sp.Ctl: Unhandled capture operation. Forgot to pair it with a prompt?"

type role Eff nominal representational
newtype Eff (es :: [Effect]) (a :: Type) = Eff { unEff :: Env es -> Ctl es a }

instance Functor (Eff es) where
  fmap = liftM

instance Applicative (Eff es) where
  pure x = Eff \_ -> pure x
  (<*>) = ap

instance Monad (Eff es) where
  Eff x >>= f = Eff \es -> Ctl $ unCtl (x es) >>= \case
    Pure a                -> unCtl $ unEff (f a) es
    Abort mark r          -> pure $ Abort mark r
    Capture mark ctl cont -> pure $ Capture mark ctl \v -> cont v >>= f

--- VM operations ---

prompt :: ∀ es a. (Marker es a -> Eff es a) -> Eff es a
prompt f = do
  mark <- freshMarker @es @a
  promptWith mark (f mark)

promptWith :: Marker es a -> Eff es a -> Eff es a
promptWith !mark (Eff m) = Eff \es -> Ctl $ unCtl (m es) >>= \case
   Pure a -> pure $ Pure a
   Abort mark' r -> case eqMarker mark mark' of
    Just Refl -> unCtl $ unEff r es
    Nothing   -> pure $ Abort mark' r
   Capture mark' ctl cont -> case eqMarker mark mark' of
    Just Refl -> unCtl $ unEff (ctl (promptWith mark . cont)) es
    Nothing   -> pure $ Capture mark' ctl (promptWith mark . cont)

abortVM :: Marker esTarget r -> Eff esTarget r -> Eff es a
abortVM !mark r = Eff \_ -> Ctl $ pure $ Abort mark r

captureVM :: Marker esTarget r -> ((Eff es a -> Eff esTarget r) -> Eff esTarget r) -> Eff es a
captureVM !mark f = Eff \_ -> Ctl $ pure $ Capture mark f id

--- Handle ---

-- | The handler context. This allows delimited control and scoped effects.
class Handling (esSend :: [Effect]) (e :: Effect) (es :: [Effect]) (r :: Type) | esSend -> e es r where
  handlingMarker :: Marker es r
  handlingMarker = error
    "Sp.Eff: nonexistent handling context! Don't attempt to manually define an instance for the 'Handling' typeclass."

type instance DictRep (Handling _ _ es r) = Marker es r

-- | The type of effect handlers.
type Handler e es r = ∀ esSend a. Handling esSend e es r => e :> esSend => e (Eff esSend) a -> Eff esSend a

unsafeIO :: IO a -> Eff es a
unsafeIO m = Eff (const $ liftIO m)
{-# INLINE unsafeIO #-}

toInternalHandler :: ∀ e es r. Marker es r -> Env es -> Handler e es r -> InternalHandler e
toInternalHandler mark es hdl = InternalHandler es \(e :: e (Eff esSend) a) ->
  reflectDict @(Handling esSend e es r) hdl mark e

alter :: (Env es' -> Env es) -> Eff es a -> Eff es' a
alter f (Eff m) = Eff \es' -> Ctl $ unCtl (m $! f es') >>= \case
  Pure a                -> pure $ Pure a
  Abort mark r          -> pure $ Abort mark r
  Capture mark ctl cont -> pure $ Capture mark ctl (alter f . cont)

handle :: Handler e es' a -> (InternalHandler e -> Env es' -> Env es) -> Eff es a -> Eff es' a
handle hdl f (Eff m) = prompt \mark -> Eff \es' ->
  Ctl $ unCtl (m $! f (toInternalHandler mark es' hdl) es') >>= \case
    Pure a                 -> pure $ Pure a
    Abort mark' r          -> pure $ Abort mark' r
    Capture mark' ctl cont -> pure $ Capture mark' ctl (handle hdl f . cont)

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

--- Control ---

-- | Perform an operation from the handle-site.
--
embed :: ∀ esSend e es r a. (Handling esSend e es r, e :> esSend) => Eff es a -> Eff esSend a
embed (Eff m) = Eff \esSend -> case Rec.index @e esSend of
  InternalHandler (es :: Env esHandle) _ ->
    case axiom @es @esHandle of
      Refl -> Ctl $ unCtl (m es) >>= \case
        Pure a                 -> pure $ Pure a
        Abort mark' r          -> pure $ Abort mark' r
        Capture mark' ctl cont -> pure $ Capture mark' ctl (embed . cont)
{-# INLINE embed #-}

-- | Abort with a result value.
abort :: ∀ esSend e es r a. Handling esSend e es r => Eff es r -> Eff esSend a
abort = abortVM (handlingMarker @esSend)
{-# INLINE abort #-}

-- | Yield and gain control of the resumption.
control
  :: ∀ esSend e es r a
  .  Handling esSend e es r
  => ((Eff esSend a -> Eff es r) -> Eff es r)
  -> Eff esSend a
control = captureVM (handlingMarker @esSend)
{-# INLINE control #-}

--- Misc ---

-- | Unpack the 'Eff' monad.
runEff :: Eff '[] a -> a
runEff (Eff m) = unsafePerformIO (runCtl $ m Rec.empty)
{-# INLINE runEff #-}

-- | Ability to embed 'IO' side effects.
data IOE :: Effect

instance IOE :> es => MonadIO (Eff es) where
  liftIO = unsafeIO
  {-# INLINE liftIO #-}

-- | Unpack an 'Eff' monad with 'IO' acitons.
runIOE :: Eff '[IOE] a -> IO a
runIOE m = runCtl $ unEff (interpret (\case) m) Rec.empty
{-# INLINE runIOE #-}
