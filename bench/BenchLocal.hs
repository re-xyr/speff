-- Benchmarking scoped effects #1: Local environments
module BenchLocal where

import qualified Control.Carrier.Error.Either as F
import qualified Control.Carrier.Reader       as F
import qualified Polysemy                     as P
import qualified Polysemy.Reader              as P
import qualified Sp.Eff                       as S
import qualified Sp.Util                      as S

programSp :: S.Reader Int S.:> es => Int -> S.Eff es Int
programSp = \case
  0 -> S.ask
  n -> S.local @Int (+1) (programSp (n - 1))
{-# NOINLINE programSp #-}

localSp :: Int -> Int
localSp n = S.runEff $ S.runReader @Int 0 $ programSp n

localSpDeep :: Int -> Int
localSpDeep n = S.runEff $ run $ run $ run $ run $ run $ S.runReader @Int 0 $ run $ run $ run $ run $ run $ programSp n
  where run = S.runReader ()

programSem :: P.Reader Int `P.Member` es => Int -> P.Sem es Int
programSem = \case
  0 -> P.ask
  n -> P.local @Int (+1) (programSem (n - 1))
{-# NOINLINE programSem #-}

localSem :: Int -> Int
localSem n = P.run $ P.runReader @Int 0 $ programSem n

localSemDeep :: Int -> Int
localSemDeep n = P.run $ run $ run $ run $ run $ run $ P.runReader @Int 0 $ run $ run $ run $ run $ run $ programSem n
  where run = P.runReader ()

programFused :: F.Has (F.Reader Int) sig m => Int -> m Int
programFused = \case
  0 -> F.ask
  n -> F.local @Int (+1) (programFused (n - 1))
{-# NOINLINE programFused #-}

localFused :: Int -> Int
localFused n = F.run $ F.runReader @Int 0 $ programFused n

localFusedDeep :: Int -> Int
localFusedDeep n = F.run $ run $ run $ run $ run $ run $ F.runReader @Int 0 $ run $ run $ run $ run $ run $ programFused n
  where run = F.runReader ()

