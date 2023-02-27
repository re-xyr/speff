-- Benchmarking scoped effects #1: Catching errors
module BenchCatch where

import qualified Control.Carrier.Error.Either as F
import qualified Control.Carrier.Reader       as F
import qualified Effectful                    as EL
import qualified Effectful.Error.Dynamic      as EL
import qualified Effectful.Reader.Dynamic     as EL
import qualified Polysemy                     as P
import qualified Polysemy.Error               as P
import qualified Polysemy.Reader              as P
import qualified Sp.Eff                       as S
import qualified Sp.Util                      as S

programSp :: S.Error () S.:> es => Int -> S.Eff es a
programSp = \case
  0 -> S.throwError ()
  n -> S.catchError (programSp (n - 1)) \() -> S.throwError ()
{-# NOINLINE programSp #-}

catchSp :: Int -> Either () ()
catchSp n = S.runEff $ S.runError $ programSp n

catchSpDeep :: Int -> Either () ()
catchSpDeep n = S.runEff $ run $ run $ run $ run $ run $ S.runError $ run $ run $ run $ run $ run $ programSp n
  where run = S.runReader ()

programEffectful :: EL.Error () EL.:> es => Int -> EL.Eff es a
programEffectful = \case
  0 -> EL.throwError ()
  n -> EL.catchError (programEffectful (n - 1)) \_ () -> EL.throwError ()
{-# NOINLINE programEffectful #-}

catchEffectful :: Int -> Either (EL.CallStack, ()) ()
catchEffectful n = EL.runPureEff $ EL.runError $ programEffectful n

catchEffectfulDeep :: Int -> Either (EL.CallStack, ()) ()
catchEffectfulDeep n =
  EL.runPureEff $ run $ run $ run $ run $ run $ EL.runError $ run $ run $ run $ run $ run $ programEffectful n
  where run = EL.runReader ()

programFused :: F.Has (F.Error ()) sig m => Int -> m a
programFused = \case
  0 -> F.throwError ()
  n -> F.catchError (programFused (n - 1)) \() -> F.throwError ()
{-# NOINLINE programFused #-}

catchFused :: Int -> Either () ()
catchFused n = F.run $ F.runError $ programFused n

catchFusedDeep :: Int -> Either () ()
catchFusedDeep n = F.run $ run $ run $ run $ run $ run $ F.runError $ run $ run $ run $ run $ run $ programFused n
  where run = F.runReader ()

programSem :: P.Error () `P.Member` es => Int -> P.Sem es a
programSem = \case
  0 -> P.throw ()
  n -> P.catch (programSem (n - 1)) \() -> P.throw ()
{-# NOINLINE programSem #-}

catchSem :: Int -> Either () ()
catchSem n = P.run $ P.runError $ programSem n

catchSemDeep :: Int -> Either () ()
catchSemDeep n = P.run $ run $ run $ run $ run $ run $ P.runError $ run $ run $ run $ run $ run $ programSem n
  where run = P.runReader ()
