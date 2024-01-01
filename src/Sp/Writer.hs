module Sp.Writer
  ( -- * Writer
    Writer (..)
  , tell
  , listen
  , runWriter
  ) where

import           Data.Foldable     (for_)
import           Data.IORef        (IORef, modifyIORef', readIORef)
import           Data.Kind         (Type)
import           Sp.Eff
import           Sp.Internal.Monad (unsafeIO, unsafeState)

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
handleWriter rs _ = \case
  Tell x   -> for_ rs \r -> unsafeIO (modifyIORef' r (<> x))
  Listen m -> unsafeState mempty \r -> do
    x <- replace (handleWriter $ r : rs) m
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
