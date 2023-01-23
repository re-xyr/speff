module Ef.Util where

import           Control.Applicative (Applicative (liftA2))
import           Data.Atomics        (atomicModifyIORefCAS)
import           Data.IORef          (IORef, newIORef, readIORef, writeIORef)
import           Data.Kind           (Type)
import           Data.Tuple          (swap)
import           Ef.Internal.Env     ((:>))
import qualified Ef.Internal.Env     as Rec
import           Ef.Internal.Monad

data Reader (r :: Type) :: Effect where
  Ask :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

-- | Obtain the environment value.
ask :: Reader r :> es => Eff es r
ask = send Ask

-- | Override the environment value in a local scope.
local :: Reader r :> es => (r -> r) -> Eff es a -> Eff es a
local f m = send (Local f m)

handleReader :: r -> Handler (Reader r) es a
handleReader !r = \case
  Ask       -> pure r
  Local f m -> interpose (handleReader (f r)) m

-- | Run the 'Reader' effect with an environment value.
runReader :: r -> Eff (Reader r : es) a -> Eff es a
runReader r = interpret (handleReader r)

-- | Provides a mutable state of type @s@.
data State s :: Effect where
  Get :: State s m s
  Put :: s -> State s m ()
  State :: (s -> (a, s)) -> State s m a

-- | Get the mutable state.
get :: State s :> es => Eff es s
get = send Get

-- | Write a new value to the mutable state.
put :: State s :> es => s -> Eff es ()
put x = send (Put x)

-- | Apply a function of type @s -> (a, s)@ on the mutable state, using the returned @s@ as the new state and
-- returning the @a@.
state :: State s :> es => (s -> (a, s)) -> Eff es a
state f = send (State f)

handleState :: IORef s -> Handler (State s) es a
handleState r = \case
  Get     -> unsafeIO (readIORef r)
  Put s   -> unsafeIO (writeIORef r s)
  State f -> unsafeIO (atomicModifyIORefCAS r (swap . f))

evalState :: s -> Eff (State s : es) a -> Eff es a
evalState s0 (Eff m) = do
  ref <- unsafeIO (newIORef s0)
  prompt \mark -> Eff \es ->
    Ctl $ unCtl (m $! Rec.cons (toInternalHandler mark es (handleState ref)) es) >>= \case
      Pure a                 -> pure $ Pure a
      Abort mark' r          -> pure $ Abort mark' r
      Capture mark' ctl cont -> do
        s1 <- readIORef ref
        pure $ Capture mark' ctl (evalState s1 . cont)

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s0 m = evalState s0 (liftA2 (,) m get)

-- | Allows you to throw error values of type @e@ and catching these errors too.
data Error (e :: Type) :: Effect where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a

-- | Throw an error.
throw :: Error e :> es => e -> Eff es a
throw e = send (Throw e)

-- | Catch any error thrown by a computation and handle it with a function.
catch :: Error e :> es => Eff es a -> (e -> Eff es a) -> Eff es a
catch m h = send (Catch m h)

-- | Catch any error thrown by a computation and return the result as an 'Either'.
try :: Error e :> es => Eff es a -> Eff es (Either e a)
try m = catch (Right <$> m) (pure . Left)

handleError :: ∀ e es a. Handler (Error e) es (Either e a)
handleError = \case
  Throw e   -> abort (pure $ Left e)
  Catch m f -> either f pure =<< interpose (handleError @e) (Right <$> m)

-- | Run the 'Error' effect. If there is any unhandled error, it is returned as a 'Left'.
runError :: ∀ e es a. Eff (Error e : es) a -> Eff es (Either e a)
runError = interpret (handleError @e) . fmap Right

-- | Provides an append-only state, and also allows you to record what is appended in a specific scope.
data Writer (w :: Type) :: Effect where
  Tell :: w -> Writer w m ()
  Listen :: m a -> Writer w m (a, w)

-- | Append a value to the state.
tell :: Writer w :> es => w -> Eff es ()
tell x = send (Tell x)

-- | Record what is appended in a specific scope.
listen :: Writer w :> es => Eff es a -> Eff es (a, w)
listen m = send (Listen m)

handleWriter :: ∀ w es a. (State w :> es, Monoid w) => Handler (Writer w) es a
handleWriter = \case
  Tell x   -> state (\xs -> ((), xs <> x))
  Listen m -> interpose
{-# INLINABLE handleWriter #-}

-- | Run the 'Writer' state, with the append-only state as a monoidal value.
runWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runWriter m = unsafeState mempty \r -> do
  x <- interpret (handleWriter [r]) m
  w' <- unsafeIO (readIORef r)
  pure (x, w')
{-# INLINABLE runWriter #-}
