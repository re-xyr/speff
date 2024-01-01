module Sp.Error
  ( -- * Error
    Error (..)
  , throwError
  , tryError
  , catchError
  , runError
  ) where

import           Data.Kind (Type)
import           Sp.Eff

-- | Allows you to throw error values of type @e@ and catching these errors too.
data Error (e :: Type) :: Effect where
  ThrowError :: e -> Error e m a
  TryError :: m a -> Error e m (Either e a)

-- | Throw an error.
throwError :: Error e :> es => e -> Eff es a
throwError e = send (ThrowError e)

-- | Catch any error thrown by a computation and return the result as an 'Either'.
tryError :: Error e :> es => Eff es a -> Eff es (Either e a)
tryError m = send (TryError m)

-- | Catch any error thrown by a computation and handle it with a function.
catchError :: Error e :> es => Eff es a -> (e -> Eff es a) -> Eff es a
catchError m h = tryError m >>= either h pure

handleError :: ∀ e es a. Handler (Error e) es (Either e a)
handleError tag = \case
  ThrowError e -> abort tag (pure $ Left e)
  TryError m   -> replace (handleError @e) (Right <$> m)

-- | Run the 'Error' effect. If there is any unhandled error, it is returned as a 'Left'.
runError :: ∀ e es a. Eff (Error e : es) a -> Eff es (Either e a)
runError = interpret (handleError @e) . fmap Right
