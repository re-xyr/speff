{-# LANGUAGE CPP #-}
-- Benchmarking effect invocation and monadic bind
module BenchCountdown where

import qualified Control.Carrier.Reader       as F
import qualified Control.Carrier.State.Strict as F
import qualified Control.Ev.Eff               as E
import qualified Control.Ev.Util              as E
#if __GLASGOW_HASKELL__ < 906
import qualified Control.Monad.Freer          as FS
import qualified Control.Monad.Freer.Reader   as FS
import qualified Control.Monad.Freer.State    as FS
#endif
import qualified Control.Monad.Identity       as M
import qualified Control.Monad.Reader         as M
import qualified Control.Monad.State.Strict   as M
import qualified Effectful                    as EL
import qualified Effectful.Reader.Dynamic     as EL
import qualified Effectful.State.Dynamic      as EL
import qualified Polysemy                     as P
import qualified Polysemy.Reader              as P
import qualified Polysemy.State               as P
import qualified Sp.Eff                       as S
import qualified Sp.Util                      as S

programSp :: S.State Int S.:> es => S.Eff es Int
programSp = do
  x <- S.get @Int
  if x == 0
    then pure x
    else do
      S.put (x - 1)
      programSp
{-# NOINLINE programSp #-}

countdownSp :: Int -> (Int, Int)
countdownSp n = S.runEff $ S.runState n programSp

countdownSpDeep :: Int -> (Int, Int)
countdownSpDeep n = S.runEff $ runR $ runR $ runR $ runR $ runR $ S.runState n $ runR $ runR $ runR $ runR $ runR $ programSp
  where runR = S.runReader ()

programEffectful :: EL.State Int EL.:> es => EL.Eff es Int
programEffectful = do
  x <- EL.get @Int
  if x == 0
    then pure x
    else do
      EL.put (x - 1)
      programEffectful
{-# NOINLINE programEffectful #-}

countdownEffectful :: Int -> (Int, Int)
countdownEffectful n = EL.runPureEff $ EL.runStateLocal n programEffectful

countdownEffectfulDeep :: Int -> (Int, Int)
countdownEffectfulDeep n =
  EL.runPureEff $ runR $ runR $ runR $ runR $ runR $ EL.runStateLocal n $ runR $ runR $ runR $ runR $ runR $ programEffectful
  where runR = EL.runReader ()

programEv :: E.State Int E.:? es => E.Eff es Int
programEv = do
  x <- E.perform (E.get @Int) ()
  if x == 0
    then pure x
    else do
      E.perform E.put (x - 1)
      programEv
{-# NOINLINE programEv #-}

countdownEv :: Int -> Int
countdownEv n = E.runEff $ E.state n programEv

countdownEvDeep :: Int -> Int
countdownEvDeep n = E.runEff $ runR $ runR $ runR $ runR $ runR $ E.state n $ runR $ runR $ runR $ runR $ runR $ programEv
  where runR = E.reader ()

#if __GLASGOW_HASKELL__ < 906
programFreer :: FS.Member (FS.State Int) es => FS.Eff es Int
programFreer = do
  x <- FS.get @Int
  if x == 0
    then pure x
    else do
      FS.put (x - 1)
      programFreer
{-# NOINLINE programFreer #-}

countdownFreer :: Int -> (Int, Int)
countdownFreer n = FS.run $ FS.runState n programFreer

countdownFreerDeep :: Int -> (Int, Int)
countdownFreerDeep n = FS.run $ runR $ runR $ runR $ runR $ runR $ FS.runState n $ runR $ runR $ runR $ runR $ runR $ programFreer
  where runR = FS.runReader ()
#endif

programMtl :: M.MonadState Int m => m Int
programMtl = do
  x <- M.get @Int
  if x == 0
    then pure x
    else do
      M.put (x - 1)
      programMtl
{-# NOINLINE programMtl #-}

countdownMtl :: Int -> (Int, Int)
countdownMtl n = M.runState programMtl n

countdownMtlDeep :: Int -> (Int, Int)
countdownMtlDeep n = M.runIdentity $ runR $ runR $ runR $ runR $ runR $ M.runStateT (runR $ runR $ runR $ runR $ runR $ programMtl) n
  where runR = (`M.runReaderT` ())

programFused :: F.Has (F.State Int) sig m => m Int
programFused = do
  x <- F.get @Int
  if x == 0
    then pure x
    else do
      F.put (x - 1)
      programFused
{-# NOINLINE programFused #-}

countdownFused :: Int -> (Int, Int)
countdownFused n = F.run $ F.runState n programFused

countdownFusedDeep :: Int -> (Int, Int)
countdownFusedDeep n = F.run $ runR $ runR $ runR $ runR $ runR $ F.runState n $ runR $ runR $ runR $ runR $ runR $ programFused
  where runR = F.runReader ()

programSem :: P.Member (P.State Int) es => P.Sem es Int
programSem = do
  x <- P.get @Int
  if x == 0
    then pure x
    else do
      P.put (x - 1)
      programSem
{-# NOINLINE programSem #-}

countdownSem :: Int -> (Int, Int)
countdownSem n = P.run $ P.runState n programSem

countdownSemDeep :: Int -> (Int, Int)
countdownSemDeep n = P.run $ runR $ runR $ runR $ runR $ runR $ P.runState n $ runR $ runR $ runR $ runR $ runR $ programSem
  where runR = P.runReader ()
