{-# LANGUAGE CPP #-}
module Main where

import           BenchCatch
import           BenchCountdown
import           BenchLocal
import           BenchPyth
import           Data.Functor     ((<&>))
import           Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bgroup "countdown" $ [10000] <&> \x -> bgroup (show x)
    [ bench "sp.shallow" $ nf countdownSp x
    , bench "sp.deep" $ nf countdownSpDeep x
    , bench "effectful.shallow" $ nf countdownEffectful x
    , bench "effectful.deep" $ nf countdownEffectfulDeep x
    , bench "ev.shallow" $ nf countdownEv x
    , bench "ev.deep" $ nf countdownEvDeep x
#if SPEFF_BENCH_FREER_SIMPLE
    , bench "freer.shallow" $ nf countdownFreer x
    , bench "freer.deep" $ nf countdownFreerDeep x
#endif
    , bench "mtl.shallow" $ nf countdownMtl x
    , bench "mtl.deep" $ nf countdownMtlDeep x
    , bench "fused.shallow" $ nf countdownFused x
    , bench "fused.deep" $ nf countdownFusedDeep x
    , bench "sem.shallow" $ nf countdownSem x
    , bench "sem.deep" $ nf countdownSemDeep x
    ]
  , bgroup "pyth" $ [32] <&> \x -> bgroup (show x)
    [ bench "sp.shallow" $ nf pythSp x
    , bench "sp.deep" $ nf pythSpDeep x
    , bench "ev.shallow" $ nf pythEv x
    , bench "ev.deep" $ nf pythEvDeep x
#ifdef SPEFF_BENCH_FREER_SIMPLE
    , bench "freer.shallow" $ nf pythFreer x
    , bench "freer.deep" $ nf pythFreerDeep x
#endif
    , bench "fused.shallow" $ nf pythFused x
    , bench "fused.deep" $ nf pythFusedDeep x
    , bench "sem.shallow" $ nf pythSem x
    , bench "sem.deep" $ nf pythSemDeep x
    ]
  , bgroup "catch" $ [10000] <&> \x -> bgroup (show x)
    [ bench "sp.shallow" $ nf catchSp x
    , bench "sp.deep" $ nf catchSpDeep x
    , bench "effectful.shallow" $ nf catchEffectful x
    , bench "effectful.deep" $ nf catchEffectfulDeep x
    , bench "fused.shallow" $ nf catchFused x
    , bench "fused.deep" $ nf catchFusedDeep x
    , bench "sem.shallow" $ nf catchSem x
    , bench "sem.deep" $ nf catchSemDeep x
    ]
  , bgroup "local" $ [10000] <&> \x -> bgroup (show x)
    [ bench "sp.shallow" $ nf localSp x
    , bench "sp.deep" $ nf localSpDeep x
    , bench "effectful.shallow" $ nf localEffectful x
    , bench "effectful.deep" $ nf localEffectfulDeep x
    , bench "fused.shallow" $ nf localFused x
    , bench "fused.deep" $ nf localFusedDeep x
    , bench "sem.shallow" $ nf localSem x
    , bench "sem.deep" $ nf localSemDeep x
    ]
  ]
