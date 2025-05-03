module Main where

import qualified Data.Set as Set
import Haal.Automaton.MealyAutomaton
import Haal.EquivalenceOracle.WMethod
import Haal.Experiment
import Haal.Learning.LMstar

-- Define input, output, and state types
data Input = A | B deriving (Show, Eq, Ord, Enum, Bounded)
data Output = X | Y deriving (Show, Eq, Ord, Enum, Bounded)
data State = S0 | S1 | S2 deriving (Show, Eq, Ord, Enum, Bounded)

-- Define the transition function for the system under learning
sulTransitions S0 _ = (S1, X)
sulTransitions S1 _ = (S2, Y)
sulTransitions S2 A = (S0, X)
sulTransitions S2 B = (S0, Y)

-- Set up the experiment.
myexperiment = experiment (mkLMstar Star) (mkWMethod 2)

-- Define the Mealy system under learning. Remember that automata can act as suls.
mysul = mkMealyAutomaton2 sulTransitions (Set.fromList [S0, S1, S2]) S0

-- Run the experiment
(learnedmodel, stats) = runExperiment myexperiment mysul

main = do 
    putStrLn "Learning Experiment"
    putStrLn "==================="
    putStrLn $ "System Under Learning: " ++ show mysul
    putStrLn $ "Learned Model: " ++ show learnedmodel
    putStrLn $ "Experiment Statistics: " ++ show stats
