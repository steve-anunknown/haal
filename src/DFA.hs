module DFA
  ( DFA,
    dfaWalk,
    dfaStep,
  )
where

import MealyAutomaton (MealyAutomaton, mealyStep, mealyWalk)

type DFA input state = MealyAutomaton input Bool state

dfaWalk :: (Ord i, Ord s) => DFA i s -> [i] -> (DFA i s, [Bool])
dfaWalk = mealyWalk

dfaStep :: (Ord i, Ord s) => DFA i s -> i -> (DFA i s, Bool)
dfaStep = mealyStep
