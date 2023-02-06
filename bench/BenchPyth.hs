-- Benchmarking yield-intensive code
module BenchPyth where

import qualified Control.Algebra               as F
import           Control.Applicative           (Alternative (empty, (<|>)))
import qualified Control.Carrier.NonDet.Church as F
import qualified Control.Carrier.Reader        as F
import qualified Control.Ev.Eff                as E
import qualified Control.Ev.Util               as E
import qualified Control.Monad.Freer           as FS
import qualified Control.Monad.Freer.NonDet    as FS
import qualified Control.Monad.Freer.Reader    as FS
import qualified Polysemy                      as P
import qualified Polysemy.NonDet               as P
import qualified Polysemy.Reader               as P
import qualified Sp.Eff                        as S
import qualified Sp.Util                       as S

programSp :: (S.NonDet S.:> e) => Int -> S.Eff e (Int, Int, Int)
programSp upbound = do
  x <- S.choice [1..upbound]
  y <- S.choice [1..upbound]
  z <- S.choice [1..upbound]
  if x*x + y*y == z*z then return (x,y,z) else S.send S.Empty
{-# NOINLINE programSp #-}

pythSp :: Int -> [(Int, Int, Int)]
pythSp n = S.runEff $ S.runNonDet $ programSp n

pythSpDeep :: Int -> [(Int, Int, Int)]
pythSpDeep n = S.runEff $ run $ run $ run $ run $ run $ S.runNonDet $ run $ run $ run $ run $ run $ programSp n
  where run = S.runReader ()

programEv :: (E.Choose E.:? e) => Int -> E.Eff e (Int, Int, Int)
programEv upbound = do
  x <- E.perform E.choose upbound
  y <- E.perform E.choose upbound
  z <- E.perform E.choose upbound
  if x*x + y*y == z*z then return (x,y,z) else E.perform (\r -> E.none r) ()
{-# NOINLINE programEv #-}

pythEv :: Int -> [(Int, Int, Int)]
pythEv n = E.runEff $ E.chooseAll $ programEv n

pythEvDeep :: Int -> [(Int, Int, Int)]
pythEvDeep n = E.runEff $ run $ run $ run $ run $ run $ E.chooseAll $ run $ run $ run $ run $ run $ programEv n
  where run = E.reader ()

programSem :: P.Member P.NonDet es => Int -> P.Sem es (Int, Int, Int)
programSem upbound = do
  x <- choice upbound
  y <- choice upbound
  z <- choice upbound
  if x*x + y*y == z*z then return (x,y,z) else empty
  where
    choice 0 = empty
    choice n = choice (n - 1) <|> pure n
{-# NOINLINE programSem #-}

pythSem :: Int -> [(Int, Int, Int)]
pythSem n = P.run $ P.runNonDet $ programSem n

pythSemDeep :: Int -> [(Int, Int, Int)]
pythSemDeep n = P.run $ run $ run $ run $ run $ run $ P.runNonDet $ run $ run $ run $ run $ run $ programSem n
  where run = P.runReader ()

programFused :: (Monad m, Alternative m) => Int -> m (Int, Int, Int)
programFused upbound = do
  x <- choice upbound
  y <- choice upbound
  z <- choice upbound
  if x*x + y*y == z*z then return (x,y,z) else empty
  where choice x = F.oneOf [1..x]
{-# NOINLINE programFused #-}

pythFused :: Int -> [(Int, Int, Int)]
pythFused n = F.run $ F.runNonDetA $ programFused n

pythFusedDeep :: Int -> [(Int, Int, Int)]
pythFusedDeep n = F.run $ run $ run $ run $ run $ run $ F.runNonDetA $ run $ run $ run $ run $ run $ programFused n
  where run = F.runReader ()

programFreer :: FS.Member FS.NonDet es => Int -> FS.Eff es (Int, Int, Int)
programFreer upbound = do
  x <- choice upbound
  y <- choice upbound
  z <- choice upbound
  if x*x + y*y == z*z then return (x,y,z) else empty
  where
    choice 0 = empty
    choice n = choice (n - 1) <|> pure n
{-# NOINLINE programFreer #-}

pythFreer :: Int -> [(Int, Int, Int)]
pythFreer n = FS.run $ FS.makeChoiceA $ programFreer n

pythFreerDeep :: Int -> [(Int, Int, Int)]
pythFreerDeep n = FS.run $ run $ run $ run $ run $ run $ FS.makeChoiceA $ run $ run $ run $ run $ run $ programFreer n
  where run = FS.runReader ()
