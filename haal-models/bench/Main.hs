module Main (main) where

import Test.Tasty.Bench (defaultMain)

import Bench.Models (modelBenchmarks)
import Bench.Scaling (scalingBenchmarks)

main :: IO ()
main = defaultMain
    [ modelBenchmarks
    , scalingBenchmarks
    ]
