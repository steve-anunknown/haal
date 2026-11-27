{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Bench.EquivalenceOracleSizes (equivalenceOracleSizes) where

import Haal.Automaton.MealyAutomaton
import Haal.EquivalenceOracle
import Haal.EquivalenceOracle.HSIMethod
import Haal.EquivalenceOracle.WMethod
import Haal.EquivalenceOracle.WpMethod
import Test.Tasty
import Test.Tasty.HUnit

import Bench.Hopcroft

{- | Computes the sizes of all test generation methods. The results will show
when running the benchmark. I got the idea from:
https://github.com/Bodigrim/tasty-bench/issues/42
-}
equivalenceOracleSizes :: TestTree
equivalenceOracleSizes =
    testGroup
        "EquivalenceOracleSizes"
        [ testGroup
            modelName
            [ testGroup
                ("depth=" ++ show depth)
                [testCaseInfo methodName (sizeInfo $ method depth model) | (methodName, method) <- testmethods]
            | depth <- [0, 1]
            ]
        | (modelName, model) <- models
        ]

{- | This computes the size of a test suite and converts it to a @IO String@,
which is needed for the `testCaseInfo`.
-}
sizeInfo :: [[i]] -> IO String
sizeInfo = pure . show . length

models :: [(String, MealyAutomaton _ _ _)]
models =
    [ ("HopcroftB2 20", hopcroftB2 20)
    , ("HopcroftB2 100", hopcroftB2 100)
    ]

testmethods :: (_) => [(String, Int -> MealyAutomaton s i o -> [[i]])]
testmethods =
    [ ("W", \d -> getTestSuite $ mkWMethod (WMethodConfig d))
    , ("Wp", \d -> getTestSuite $ mkWpMethod (WpMethodConfig d))
    , ("HSI", \d -> getTestSuite $ mkHSIMethod (WMethodConfig d))
    ]
  where
    fromRight = either error id
    getTestSuite oracle = snd . testSuite (fromRight oracle)
