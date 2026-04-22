module Main (main) where

import Test.Tasty.Bench (defaultMain)

import Bench.BlackBox (blackBoxBenchmarks)
import Bench.Dot (dotBenchmarks)
import Bench.EndToEnd (endToEndBenchmarks)
import Bench.EquivalenceOracle (equivalenceOracleBenchmarks)

main :: IO ()
main = defaultMain
    [ blackBoxBenchmarks
    , equivalenceOracleBenchmarks
    , dotBenchmarks
    , endToEndBenchmarks
    ]
