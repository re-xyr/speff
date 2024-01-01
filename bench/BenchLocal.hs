{-# LANGUAGE CPP #-}
-- Benchmarking scoped effects #2: Local environments
module BenchLocal where

import qualified Control.Carrier.Error.Either as F
import qualified Control.Carrier.Reader       as F
#if SPEFF_BENCH_EFFECTFUL
import qualified Effectful                    as EL
import qualified Effectful.Reader.Dynamic     as EL
#endif
import qualified Polysemy                     as P
import qualified Polysemy.Reader              as P
import qualified Sp.Eff                       as S
import qualified Sp.Reader                    as S

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

#if SPEFF_BENCH_EFFECTFUL
programEffectful :: EL.Reader Int EL.:> es => Int -> EL.Eff es Int
programEffectful = \case
  0 -> EL.ask
  n -> EL.local @Int (+1) (programEffectful (n - 1))
{-# NOINLINE programEffectful #-}

localEffectful :: Int -> Int
localEffectful n = EL.runPureEff $ EL.runReader @Int 0 $ programEffectful n

localEffectfulDeep :: Int -> Int
localEffectfulDeep n =
  EL.runPureEff $ run $ run $ run $ run $ run $ EL.runReader @Int 0 $ run $ run $ run $ run $ run $ programEffectful n
  where run = EL.runReader ()
#endif

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
