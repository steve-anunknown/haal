{-# LANGUAGE DeriveGeneric #-}

module Bench.EndToEnd (endToEndBenchmarks) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

import Haal.Automaton.MealyAutomaton (MealyAutomaton, mkMealyAutomaton)
import Haal.EquivalenceOracle.WMethod (WMethod, WMethodConfig (..), mkWMethod)
import Haal.EquivalenceOracle.WpMethod (WpMethod, WpMethodConfig (..), mkWpMethod)
import Haal.Experiment (experiment, runExperiment)
import Haal.Learning.LMstar (LMstarConfig (..), mkLMstar)

-- | Input alphabet for benchmark automata.
data In = IA | IB | IC | ID deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance NFData In

-- | Output alphabet for benchmark automata.
data Out = OX | OY | OZ | OW deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance NFData Out

-- | States for the SUL (target automaton).
data St = S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance NFData St

-- | A deterministic 8-state Mealy automaton used as the SUL.
benchAutomaton :: MealyAutomaton St In Out
benchAutomaton = mkMealyAutomaton delta lambda (Set.fromList [S0 .. S7]) S0
  where
    transMap :: Map.Map (St, In) (St, Out)
    transMap = Map.fromList
        [ ((S0, IA), (S1, OX)), ((S0, IB), (S2, OY)), ((S0, IC), (S3, OZ)), ((S0, ID), (S4, OW))
        , ((S1, IA), (S5, OY)), ((S1, IB), (S0, OX)), ((S1, IC), (S6, OW)), ((S1, ID), (S7, OZ))
        , ((S2, IA), (S3, OZ)), ((S2, IB), (S4, OX)), ((S2, IC), (S7, OY)), ((S2, ID), (S1, OW))
        , ((S3, IA), (S6, OW)), ((S3, IB), (S5, OZ)), ((S3, IC), (S0, OX)), ((S3, ID), (S2, OY))
        , ((S4, IA), (S7, OX)), ((S4, IB), (S6, OW)), ((S4, IC), (S1, OY)), ((S4, ID), (S0, OZ))
        , ((S5, IA), (S2, OZ)), ((S5, IB), (S7, OY)), ((S5, IC), (S4, OX)), ((S5, ID), (S3, OW))
        , ((S6, IA), (S4, OY)), ((S6, IB), (S1, OX)), ((S6, IC), (S5, OZ)), ((S6, ID), (S0, OW))
        , ((S7, IA), (S0, OW)), ((S7, IB), (S3, OZ)), ((S7, IC), (S2, OY)), ((S7, ID), (S6, OX))
        ]

    delta s i = fst (transMap Map.! (s, i))
    lambda s i = snd (transMap Map.! (s, i))

wmethod :: Int -> WMethod
wmethod d = either error id (mkWMethod (WMethodConfig d))

wpmethod :: Int -> WpMethod
wpmethod d = either error id (mkWpMethod (WpMethodConfig d))

-- | Run a full learning experiment (init -> learn -> test -> refine -> converge).
-- Uses whnf since MealyAutomaton contains functions that cannot be NFData.
endToEndBenchmarks :: Benchmark
endToEndBenchmarks = bgroup "EndToEnd"
    [ bgroup "LMstar"
        [ bench "WMethod/depth=1" $
            whnf (runExperiment (experiment (mkLMstar Star) (wmethod 1))) benchAutomaton
        , bench "WpMethod/depth=1" $
            whnf (runExperiment (experiment (mkLMstar Star) (wpmethod 1))) benchAutomaton
        ]
    , bgroup "LMplus"
        [ bench "WMethod/depth=1" $
            whnf (runExperiment (experiment (mkLMstar Plus) (wmethod 1))) benchAutomaton
        , bench "WpMethod/depth=1" $
            whnf (runExperiment (experiment (mkLMstar Plus) (wpmethod 1))) benchAutomaton
        ]
    ]
