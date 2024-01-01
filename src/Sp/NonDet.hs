{-# OPTIONS_GHC -Wno-orphans #-}
module Sp.NonDet
  ( -- * Nondeterminism
    NonDet (..)
  , choice
  , runNonDet
    -- Culling
  , Cull (..)
  , cull
  , runCull
    -- Cutting
  , Cut (..)
  , call
  , cutfail
  , cut
  , runCut
  ) where

import           Control.Applicative (Alternative (empty, (<|>)))
import           Debug.Trace
import           Sp.Eff
import           Sp.Error

-- | Provides nondeterministic choice.
data NonDet :: Effect where
  Empty :: NonDet m a
  Choice :: [a] -> NonDet m a

-- | Nondeterministic choice.
choice :: NonDet :> es => [a] -> Eff es a
choice etc = send (Choice etc)

handleNonDet :: Alternative f => Handler NonDet es (f a)
handleNonDet tag = \case
  Empty -> abort tag $ pure empty
  Choice etc -> control tag \cont ->
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

data Cull :: Effect where
  Cull :: NonDet :> esSend => Eff esSend a -> Cull (Eff esSend) a

cull :: (Cull :> es, NonDet :> es) => Eff es a -> Eff es a
cull m = send (Cull m)

handleCull :: Handler Cull es a
handleCull _tag = \case
  Cull m -> maybe empty pure
    =<< replace (handleNonDet @Maybe) (fmap Just m)

runCull :: Eff (Cull : es) a -> Eff es a
runCull = interpret handleCull

data Cut :: Effect where
  Call :: NonDet :> esSend => Eff esSend a -> Cut (Eff esSend) a
  Cutfail :: Cut m a

call :: (Cut :> es, NonDet :> es) => Eff es a -> Eff es a
call m = send (Call m)

cutfail :: Cut :> es => Eff es a
cutfail = send Cutfail

cut :: (Cut :> es, NonDet :> es) => Eff es ()
cut = pure () <|> cutfail

data CutBail a = CutBail [a]

handleCut :: forall a es. (NonDet :> es, Error (CutBail a) :> es) => Handler Cut es a
handleCut tag = \case
  Call m -> do
    err <- runError $ replace0 handleNonDetCut $ fmap (:[]) $ replace handleCut m
    case err of
      Left (CutBail xs) -> choice xs
      Right xs          -> choice xs
  Cutfail -> embed tag $ throwError $ CutBail @a []

handleNonDetCut :: forall a es. Error (CutBail a) :> es => Handler NonDet es [a]
handleNonDetCut tag = \case
  Empty -> abort tag $ pure []
  Choice etc -> control tag \cont ->
    let collect [] acc = pure acc
        collect (e : etc') acc =
          tryError (cont $ pure e) >>= \case
            Left (CutBail xs) -> trace "caught" $ throwError $ CutBail @a (acc ++ xs)
            Right xs -> collect etc' $! (acc ++ xs)
    in collect etc []

runCut :: NonDet :> es => Eff (Cut : es) a -> Eff es a
runCut m = do
  err <- runError $ interpret handleCut $ send $ Call m
  case err of
    Left (CutBail xs) -> choice xs
    Right a           -> pure a
