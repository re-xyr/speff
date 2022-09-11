-- Benchmarking scoped effects #1: Catching errors
module BenchCatch where

import qualified Control.Carrier.Error.Either as F
import qualified Control.Carrier.Reader       as F
import qualified Polysemy                     as P
import qualified Polysemy.Error               as P
import qualified Polysemy.Reader              as P
import qualified Sp.Eff                       as S
import qualified Sp.Util                      as S

programSp :: S.Error () S.:> es => Int -> S.Eff es a
programSp = \case
  0 -> S.throw ()
  n -> S.catch (programSp (n - 1)) \() -> S.throw ()
{-# NOINLINE programSp #-}

catchSp :: Int -> Either () ()
catchSp n = S.runEff $ S.runError $ programSp n

catchSpDeep :: Int -> Either () ()
catchSpDeep n = S.runEff $ run $ run $ run $ run $ run $ S.runError $ run $ run $ run $ run $ run $ programSp n
  where run = S.runReader ()

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
