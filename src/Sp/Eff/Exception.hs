-- |
-- Copyright: (c) 2023 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
--
-- Exception-related facilities for the @Eff@ monad. A large part are reexports of the barebones definitions from the
-- @exceptions@ package; for saner defaults, consider using a wrapper package like @safe-exceptions@.
module Sp.Eff.Exception
  ( -- * Throwing
    MonadThrow (throwM)
    -- * @bracket@-like operation
  , dynamicWind
    -- * Catching
  , MonadCatch (catch)
  , catchAll
  , catchIOError
  , catchJust
  , catchIf
  , Handler (..)
  , catches
  , handle
  , handleAll
  , handleIOError
  , handleJust
  , handleIf
  , try
  , tryJust
  , onException
    -- * Masking
  , mask
  , mask_
  , uninterruptibleMask
  , uninterruptibleMask_
  , interruptible
    -- * Reexports
  , Exception
  , SomeException
  ) where

import           Control.Monad.Catch (Exception, Handler (..), MonadCatch (catch), MonadThrow (throwM), SomeException,
                                      catchAll, catchIOError, catchIf, catchJust, catches, handle, handleAll,
                                      handleIOError, handleIf, handleJust, onException, try, tryJust)
import           Sp.Internal.Env     ((:>))
import           Sp.Internal.Monad   (Eff, IOE, dynamicWind, interruptible, mask, uninterruptibleMask)

-- | Lifted version of 'Control.Exception.mask_'.
mask_ :: IOE :> es => Eff es a -> Eff es a
mask_ m = mask \_ -> m
{-# INLINE mask_ #-}

-- | Lifted version of 'Control.Exception.uninterruptibleMask_'.
uninterruptibleMask_ :: IOE :> es => Eff es a -> Eff es a
uninterruptibleMask_ m = uninterruptibleMask \_ -> m
{-# INLINE uninterruptibleMask_ #-}
