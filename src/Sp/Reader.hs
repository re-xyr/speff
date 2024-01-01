module Sp.Reader
  ( -- * Reader
    Reader (..)
  , ask
  , local
  , runReader
  ) where

import           Data.Kind (Type)
import           Sp.Eff

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
handleReader !r _ = \case
  Ask       -> pure r
  Local f m -> replace (handleReader $ f r) m

-- | Run the 'Reader' effect with an environment value.
runReader :: r -> Eff (Reader r : es) a -> Eff es a
runReader r = interpret (handleReader r)
