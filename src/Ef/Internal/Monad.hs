{-# LANGUAGE AllowAmbiguousTypes #-}
module Ef.Internal.Monad where

import           Control.Exception      (Exception)
import qualified Control.Exception      as Exception
import           Control.Monad          (ap, liftM)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Atomics.Counter   (AtomicCounter, incrCounter, newCounter)
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
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

type role Marker nominal nominal representational representational
newtype Marker (es :: [Effect]) (es' :: [Effect]) (r :: Type) (r' :: Type) = Marker Int

eqMarker
  :: Marker esFrom esTo aFrom aTo
  -> Marker esFrom' esTo' aFrom' aTo'
  -> Maybe (Marker esFrom esTo aFrom aTo :~: Marker esFrom' esTo' aFrom' aTo')
eqMarker (Marker x) (Marker y) =
  if x == y then Just axiom else Nothing

freshMarker :: ∀ es es' r r'. IO (Marker es es' r r')
freshMarker = Marker <$> incrCounter 1 uniqueSource

--- Effect monad ---

type Effect = (Type -> Type) -> Type -> Type

type Env = Rec InternalHandler

type role InternalHandler nominal
data InternalHandler (e :: Effect) =
  forall (esHandle :: [Effect]). InternalHandler
  { handlerEnv :: Env esHandle
  , runHandler :: ∀ esSend a. e :> esSend => e (Eff esSend) a -> Eff esSend a
  }

data CaptureControl esSend b esTarget esTarget' r r'
  = ShiftCapture ((Eff esSend b -> Eff esTarget' r') -> Eff esTarget r)
  | ControlCapture ((Eff esSend b -> Eff esTarget r) -> Eff esTarget r)
  | Shift0Capture ((Eff esSend b -> Eff esTarget' r') -> Eff esTarget' r')
  | Control0Capture ((Eff esSend b -> Eff esTarget r) -> Eff esTarget' r')

type role Result nominal representational
data Result (es :: [Effect]) (a :: Type)
  = Pure a
  | ∀ (esTarget :: [Effect]) (esTarget' :: [Effect]) (r :: Type) (r' :: Type).
    Abort !(Marker esTarget esTarget' r r') (Eff esTarget' r')
  | ∀ (esSend :: [Effect]) (b :: Type) (esTarget :: [Effect]) (esTarget' :: [Effect]) (r :: Type) (r' :: Type).
    Capture !(Marker esTarget esTarget' r r') (CaptureControl esSend b esTarget esTarget' r r') (Eff esSend b -> Eff es a)

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

abortVM :: Marker esTarget esTarget' r r' -> Eff esTarget' r' -> Eff es a
abortVM !mark r = Eff \_ -> Ctl $ pure $ Abort mark r

captureVM :: Marker esTarget esTarget' r r' -> CaptureControl es a esTarget esTarget' r r' -> Eff es a
captureVM !mark ctl = Eff \_ -> Ctl $ pure $ Capture mark ctl id

-- | The handler context. This allows delimited control and scoped effects.
class Handling (esSend :: [Effect]) (e :: Effect) (es :: [Effect]) (es' :: [Effect]) (r :: Type) (r' :: Type) | esSend -> e es es' r r' where
  handlingMarker :: Marker es es' r r'
  handlingMarker = error
    "Sp.Eff: nonexistent handling context! Don't attempt to manually define an instance for the 'Handling' typeclass."

type instance DictRep (Handling _ _ es es' r r') = Marker es es' r r'

-- | The type of effect handlers.
type Handler e es es' r r' = ∀ esSend a. Handling esSend e es es' r r' => e :> esSend => e (Eff esSend) a -> Eff esSend a

unsafeIO :: IO a -> Eff es a
unsafeIO m = Eff (const $ liftIO m)
{-# INLINE unsafeIO #-}

toInternalHandler :: ∀ e es es' r r'. Marker es es' r r' -> Env es' -> Handler e es es' r r' -> InternalHandler e
toInternalHandler mark es' hdl = InternalHandler es' \(e :: e (Eff esSend) a) ->
  reflectDict @(Handling esSend e es es' r r') hdl mark e

promptState :: IORef s -> Eff es a -> Eff es a
promptState ref (Eff m) = Eff \es -> Ctl $ unCtl (m es) >>= \case
  Pure a -> pure $ Pure a
  Abort mark r -> pure $ Abort mark r
  Capture mark ctl cont -> do
    s <- readIORef ref
    pure $ Capture mark ctl \x -> do
      unsafeIO (writeIORef ref s)
      cont x

unsafeState :: s -> (IORef s -> Eff es a) -> Eff es a
unsafeState s0 f = do
  ref <- unsafeIO (newIORef s0)
  f ref

alter :: (Env es' -> Env es) -> Eff es a -> Eff es' a
alter f (Eff m) = Eff \es' -> Ctl $ unCtl (m $! f es') >>= \case
  Pure a                -> pure $ Pure a
  Abort mark r          -> pure $ Abort mark r
  Capture mark ctl cont -> pure $ Capture mark ctl (alter f . cont)

handle :: (InternalHandler e -> Env es' -> Env es) -> (a -> Eff es' b) -> Handler e es es' a b -> Eff es a -> Eff es' b
handle f onPure hdl m = Eff \es' -> Ctl do
  mark <- freshMarker
  unCtl (unEff m $ f (toInternalHandler mark es' hdl) es') >>= \case
    Pure a -> unCtl $ unEff (onPure a) es'
    Abort mark' r -> case eqMarker mark mark' of
      Nothing   -> pure $ Abort mark' r
      Just Refl -> unCtl $ unEff r es'
    Capture mark' ctl cont -> case eqMarker mark mark' of
      Nothing   -> pure $ Capture mark' ctl (handle f onPure hdl . cont)
      Just Refl -> unCtl case ctl of
        ShiftCapture ctl'    -> unEff (handle f onPure hdl $ ctl' (handle f onPure hdl . cont)) es'
        ControlCapture ctl'  -> unEff (handle f onPure hdl $ ctl' $ cont) es'
        Shift0Capture ctl'   -> unEff (ctl' (handle f onPure hdl . cont)) es'
        Control0Capture ctl' -> unEff (ctl' cont) es'

catch :: Exception e => Eff es a -> (e -> Eff es a) -> Eff es a
catch (Eff m) h = Eff \es -> Ctl $ Exception.catch (unCtl $ m es) \e ->
  unCtl (unEff (h e) es) >>= \case
    Pure a                -> pure $ Pure a
    Abort mark r          -> pure $ Abort mark r
    Capture mark ctl cont -> pure $ Capture mark ctl \x -> catch (cont x) h

dynamicWind :: Eff es () -> Eff es () -> Eff es a -> Eff es a
dynamicWind before after action = do
  x <- before >> Eff \es -> Ctl $ unCtl (handledAction es) >>= \case
    Pure a                -> pure $ Pure a
    Abort mark r          -> pure $ Abort mark r
    Capture mark ctl cont -> pure $ Capture mark ctl (dynamicWind before after . cont)
  after >> pure x
  where
    Eff handledAction = catch @Exception.SomeException action \e ->
      after >> unsafeIO (Exception.throwIO e)

--- Handling ---

-- | Handle an effect.
interpret :: (a -> Eff es b) -> Handler e (e : es) es a b -> Eff (e : es) a -> Eff es b
interpret = handle Rec.cons
{-# INLINE interpret #-}

-- | Handle an effect with another newly introduced effect. This allows for effect encapsulation because it does not
-- require placing constraints on the original row.
reinterpret :: (a -> Eff (e' : es) b) -> Handler e (e : es) (e' : es) a b -> Eff (e : es) a -> Eff (e' : es) b
reinterpret = handle \ih es -> Rec.cons ih $ Rec.tail es
{-# INLINE reinterpret #-}

-- | Handle an effect already in the environment. This is particularly useful in scoped effect operations.
interpose :: e :> es => (a -> Eff es b) -> Handler e es es a b -> Eff es a -> Eff es b
interpose = handle Rec.update
{-# INLINE interpose #-}

-- | Handle an effect already in the environment with another newly introduced effect.
reinterpose :: e :> es => (a -> Eff (e' : es) b) -> Handler e es (e' : es) a b -> Eff es a -> Eff (e' : es) b
reinterpose = handle \ih es -> Rec.update ih $ Rec.tail es
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

-- --- Control ---

-- | Perform an operation from the handle-site.
embed :: ∀ esSend e es es' r r' a. (Handling esSend e es es' r r', e :> esSend) => Eff es' a -> Eff esSend a
embed (Eff m) = Eff \esSend -> case Rec.index @e esSend of
  InternalHandler (es :: Env esHandle) _ ->
    case axiom @es' @esHandle of
      Refl -> Ctl $ unCtl (m es) >>= \case
        Pure a                 -> pure $ Pure a
        Abort mark' r          -> pure $ Abort mark' r
        Capture mark' ctl cont -> pure $ Capture mark' ctl (embed . cont)
{-# INLINE embed #-}

-- | Abort with a result value.
abort :: ∀ esSend e es es' r r' a. Handling esSend e es es' r r' => Eff es' r' -> Eff esSend a
abort = abortVM (handlingMarker @esSend)
{-# INLINE abort #-}

-- | Yield and gain control of the resumption.
shift0
  :: ∀ esSend e es es' r r' a
  .  Handling esSend e es es' r r'
  => ((Eff esSend a -> Eff es' r') -> Eff es' r')
  -> Eff esSend a
shift0 = captureVM (handlingMarker @esSend) . Shift0Capture
{-# INLINE shift0 #-}

-- | Yield and gain control of the resumption.
control0
  :: ∀ esSend e es es' r r' a
  .  Handling esSend e es es' r r'
  => ((Eff esSend a -> Eff es r) -> Eff es' r')
  -> Eff esSend a
control0 = captureVM (handlingMarker @esSend) . Control0Capture
{-# INLINE control0 #-}

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
runIOE m = runCtl $ unEff (interpret pure (\case) m) Rec.empty
{-# INLINE runIOE #-}
