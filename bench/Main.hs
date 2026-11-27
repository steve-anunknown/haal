module Main (main) where

import Test.Tasty.Bench (defaultMain)

import Bench.BlackBox (blackBoxBenchmarks)
import Bench.Dot (dotBenchmarks)
import Bench.EndToEnd (endToEndBenchmarks)
import Bench.EquivalenceOracle (equivalenceOracleBenchmarks)
import Bench.EquivalenceOracleSizes (equivalenceOracleSizes)

main :: IO ()
main =
    defaultMain
        [ blackBoxBenchmarks
        , equivalenceOracleBenchmarks
        , equivalenceOracleSizes
        , dotBenchmarks
        , endToEndBenchmarks
        ]
