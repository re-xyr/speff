{-# OPTIONS_GHC -Wno-orphans #-}
module Ef.Util where

import           Control.Applicative (Alternative (empty, (<|>)))
import           Data.Atomics        (atomicModifyIORefCAS, atomicModifyIORefCAS_)
import           Data.Foldable       (for_)
import           Data.IORef          (IORef, readIORef, writeIORef)
import           Data.Kind           (Type)
import           Data.Tuple          (swap)
import           Ef.Internal.Env     ((:>))
import           Ef.Internal.Monad   hiding (catch)

data Reader (r :: Type) :: Effect where
  Ask :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

-- | Obtain the environment value.
ask :: Reader r :> es => Eff es r
ask = send Ask

-- | Override the environment value in a local scope.
local :: Reader r :> es => (r -> r) -> Eff es a -> Eff es a
local f m = send (Local f m)

handleReader :: r -> Handler (Reader r) es es' a a
handleReader !r = \case
  Ask       -> pure r
  Local f m -> interpose pure (handleReader (f r)) m

-- | Run the 'Reader' effect with an environment value.
runReader :: r -> Eff (Reader r : es) a -> Eff es a
runReader r = interpret pure (handleReader r)

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

handleState :: IORef s -> Handler (State s) es es' a a
handleState r = \case
  Get     -> unsafeIO (readIORef r)
  Put s   -> unsafeIO (writeIORef r s)
  State f -> unsafeIO (atomicModifyIORefCAS r (swap . f))

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s m = unsafeState s \r -> do
  x <- interpret pure (handleState r) m
  s' <- unsafeIO (readIORef r)
  pure (x, s')

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

handleError :: ∀ e es es' a. Handler (Error e) es es' a (Either e a)
handleError = \case
  Throw e   -> abort (pure $ Left e)
  Catch m f -> either f pure =<< interpose (pure . Right) (handleError @e) m

-- | Run the 'Error' effect. If there is any unhandled error, it is returned as a 'Left'.
runError :: ∀ e es a. Eff (Error e : es) a -> Eff es (Either e a)
runError = interpret (pure . Right) (handleError @e)

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

handleWriter :: ∀ w es es' a a'. Monoid w => [IORef w] -> Handler (Writer w) es es' a a'
handleWriter rs = \case
  Tell x   -> for_ rs \r -> unsafeIO (atomicModifyIORefCAS_ r (<> x))
  Listen m -> unsafeState mempty \r' -> do
    x <- interpose pure (handleWriter (r' : rs)) m
    w' <- unsafeIO (readIORef r')
    pure (x, w')
{-# INLINABLE handleWriter #-}

-- | Run the 'Writer' state, with the append-only state as a monoidal value.
runWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runWriter m = unsafeState mempty \r -> do
  x <- interpret pure (handleWriter [r]) m
  w' <- unsafeIO (readIORef r)
  pure (x, w')
{-# INLINABLE runWriter #-}

-- | Provides nondeterministic choice.
data NonDet :: Effect where
  Empty :: NonDet m a
  Choice :: [a] -> NonDet m a

-- | Nondeterministic choice.
choice :: NonDet :> es => [a] -> Eff es a
choice etc = send (Choice etc)

handleNonDet :: Alternative f => Handler NonDet es es' a (f a)
handleNonDet = \case
  Empty -> abort $ pure empty
  Choice etc -> shift0 \cont ->
    let collect [] acc = pure acc
        collect (e : etc') acc = do
          xs <- cont (pure e)
          collect etc' $! (acc <|> xs)
    in collect etc empty
{-# INLINABLE handleNonDet #-}

-- | Run the 'NonDet' effect, with the nondeterministic choice provided by an 'Alternative' instance.
runNonDet :: Alternative f => Eff (NonDet : es) a -> Eff es (f a)
runNonDet = interpret (pure . pure) handleNonDet
{-# INLINABLE runNonDet #-}

instance NonDet :> es => Alternative (Eff es) where
  empty = send Empty
  m <|> n = do
    x <- send (Choice [True, False])
    if x then m else n
