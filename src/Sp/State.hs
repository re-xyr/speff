module Sp.State
  ( -- * State
    State (..)
  , get
  , put
  , modify
  , state
  , runState
  ) where

import           Data.Functor      (($>))
import           Data.IORef        (IORef, readIORef, writeIORef)
import           Sp.Eff
import           Sp.Internal.Monad (unsafeIO, unsafeState)


-- | Provides a mutable state of type @s@.
data State s :: Effect where
  Get :: State s m s
  Put :: s -> State s m ()
  State :: (s -> (s, a)) -> State s m a

-- | Get the mutable state.
get :: State s :> es => Eff es s
get = send Get

-- | Write a new value to the mutable state.
put :: State s :> es => s -> Eff es ()
put x = send (Put x)

-- | Apply a function to the mutable state.
modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state ((, ()) . f)

-- | Apply a function of type @s -> (s, a)@ on the mutable state, using the returned @s@ as the new state and
-- returning the @a@.
state :: State s :> es => (s -> (s, a)) -> Eff es a
state f = send (State f)

handleState :: IORef s -> Handler (State s) es a
handleState r _ = \case
  Get     -> unsafeIO (readIORef r)
  Put s   -> unsafeIO (writeIORef r s)
  State f -> unsafeIO do
    (!s1, x) <- f <$> readIORef r
    writeIORef r s1 $> x

-- | Run the 'State' effect with an initial value for the mutable state.
runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s m = unsafeState s \r -> do
  x <- interpret (handleState r) m
  s' <- unsafeIO (readIORef r)
  pure (x, s')
