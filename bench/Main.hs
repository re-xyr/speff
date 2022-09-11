import           BenchCatch
import           BenchCountdown
import           BenchLocal
import           BenchPyth
import           Data.Functor     ((<&>))
import           Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bgroup "countdown" $ [1000000] <&> \x -> bgroup (show x)
    [ bench "sp.shallow" $ nf countdownSp x
    , bench "sp.deep" $ nf countdownSpDeep x
    , bench "ev.shallow" $ nf countdownEv x
    , bench "ev.deep" $ nf countdownEvDeep x
    , bench "freer.shallow" $ nf countdownFreer x
    , bench "freer.deep" $ nf countdownFreerDeep x
    , bench "mtl.shallow" $ nf countdownMtl x
    , bench "mtl.deep" $ nf countdownMtlDeep x
    , bench "fused.shallow" $ nf countdownFused x
    , bench "fused.deep" $ nf countdownFusedDeep x
    , bench "sem.shallow" $ nf countdownSem x
    , bench "sem.deep" $ nf countdownSemDeep x
    ]
  , bgroup "pyth" $ [128] <&> \x -> bgroup (show x)
    [ bench "sp.shallow" $ nf pythSp x
    , bench "sp.deep" $ nf pythSpDeep x
    , bench "ev.shallow" $ nf pythEv x
    , bench "ev.deep" $ nf pythEvDeep x
    , bench "freer.shallow" $ nf pythFreer x
    , bench "freer.deep" $ nf pythFreerDeep x
    , bench "fused.shallow" $ nf pythFused x
    , bench "fused.deep" $ nf pythFusedDeep x
    , bench "sem.shallow" $ nf pythSem x
    , bench "sem.deep" $ nf pythSemDeep x
    ]
  , bgroup "catch" $ [1000000] <&> \x -> bgroup (show x)
    [ bench "sp.shallow" $ nf catchSp x
    , bench "sp.deep" $ nf catchSpDeep x
    , bench "fused.shallow" $ nf catchFused x
    , bench "fused.deep" $ nf catchFusedDeep x
    , bench "sem.shallow" $ nf catchSem x
    , bench "sem.deep" $ nf catchSemDeep x
    ]
  , bgroup "local" $ [1000000] <&> \x -> bgroup (show x)
    [ bench "sp.shallow" $ nf localSp x
    , bench "sp.deep" $ nf localSpDeep x
    , bench "fused.shallow" $ nf localFused x
    , bench "fused.deep" $ nf localFusedDeep x
    , bench "sem.shallow" $ nf localSem x
    , bench "sem.deep" $ nf localSemDeep x
    ]
  ]
