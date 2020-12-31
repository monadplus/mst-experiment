import Gauge
import Graph
import System.IO

setupEnv :: IO (UGraph, UGraph, UGraph, UGraph)
setupEnv =
  (,,,) <$> genGraph 128
    <*> genGraph 512
    <*> genGraph 1024
    <*> genGraph 2048

main :: IO ()
main = do
  hSetEncoding stdout utf8
  defaultMain
    [ env setupEnv $ \ ~(small, medium, big, bigger) ->
        bgroup
          "prim"
          -- We need to evaluate to normal form as prim's algorithm is nonstrict.
          [ bench "small" $ nf prim small,
            bench "medium" $ nf prim medium,
            bench "big" $ nf prim big,
            bench "bigger" $ nf prim bigger
          ]
    ]
