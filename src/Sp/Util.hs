{-# OPTIONS_GHC -Wno-orphans #-}
module Sp.Util
  ( -- * Reader
    Reader (..)
  , ask
  , local
  , runReader
    -- * State
  , State (..)
  , get
  , put
  , modify
  , state
  , runState
    -- * Error
  , Error (..)
  , throw
  , try
  , catch
  , runError
    -- * Writer
  , Writer (..)
  , tell
  , listen
  , runWriter
    -- * Nondeterminism
  , NonDet (..)
  , choice
  , runNonDet
  ) where

import           Control.Applicative (Alternative (empty, (<|>)))
import           Data.Atomics        (atomicModifyIORefCAS)
import           Data.Foldable       (for_)
import           Data.IORef          (IORef, modifyIORef', readIORef, writeIORef)
import           Data.Kind           (Type)
import           Sp.Internal.Env     ((:>))
import           Sp.Internal.Handle
import           Sp.Internal.Monad

-- | Provides an environment value of type @r@, and you can override it in a local scope.
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
handleState r = \case
  Get     -> unsafeIO (readIORef r)
  Put s   -> unsafeIO (writeIORef r s)
  State f -> unsafeIO (atomicModifyIORefCAS r f)

-- | Run the 'State' effect with an initial value for the mutable state.
runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s m = unsafeState s \r -> do
  x <- interpret (handleState r) m
  s' <- unsafeIO (readIORef r)
  pure (x, s')

-- | Allows you to throw error values of type @e@ and catching these errors too.
data Error (e :: Type) :: Effect where
  Throw :: e -> Error e m a
  Try :: m a -> Error e m (Either e a)

-- | Throw an error.
throw :: Error e :> es => e -> Eff es a
throw e = send (Throw e)

-- | Catch any error thrown by a computation and return the result as an 'Either'.
try :: Error e :> es => Eff es a -> Eff es (Either e a)
try m = send (Try m)

-- | Catch any error thrown by a computation and handle it with a function.
catch :: Error e :> es => Eff es a -> (e -> Eff es a) -> Eff es a
catch m h = try m >>= either h pure

handleError :: ∀ e es a. Handler (Error e) es (Either e a)
handleError = \case
  Throw e -> abort (pure $ Left e)
  Try m   -> interpose (handleError @e) (Right <$> m)

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

handleWriter :: ∀ w es a. Monoid w => [IORef w] -> Handler (Writer w) es a
handleWriter rs = \case
  Tell x   -> for_ rs \r -> unsafeIO (modifyIORef' r (<> x))
  Listen m -> unsafeState mempty \r -> do
    x <- interpose (handleWriter $ r : rs) m
    w' <- unsafeIO (readIORef r)
    pure (x, w')
{-# INLINABLE handleWriter #-}

-- | Run the 'Writer' state, with the append-only state as a monoidal value.
runWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runWriter m = unsafeState mempty \r -> do
  x <- interpret (handleWriter [r]) m
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

handleNonDet :: Alternative f => Handler NonDet es (f a)
handleNonDet = \case
  Empty -> abort $ pure empty
  Choice etc -> control \cont ->
    let collect [] acc = pure acc
        collect (e : etc') acc = do
          xs <- cont (pure e)
          collect etc' $! (acc <|> xs)
    in collect etc empty
{-# INLINABLE handleNonDet #-}

-- | Run the 'NonDet' effect, with the nondeterministic choice provided by an 'Alternative' instance.
runNonDet :: Alternative f => Eff (NonDet : es) a -> Eff es (f a)
runNonDet = interpret handleNonDet . fmap pure
{-# INLINABLE runNonDet #-}

instance NonDet :> es => Alternative (Eff es) where
  empty = send Empty
  m <|> n = do
    x <- send (Choice [True, False])
    if x then m else n
