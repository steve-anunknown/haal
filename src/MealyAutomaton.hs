{-# LANGUAGE ScopedTypeVariables #-}

module MealyAutomaton (
    MealyAutomaton (..),
    mkMealyAutomaton,
    mkMealyAutomaton2,
    mealyStep,
    mealyWalk,
    mealyReset,
)
where

import qualified BlackBox
import qualified Data.Data as Data
import qualified Data.List as List
import qualified Data.Map as Map

data MealyAutomaton input output state = MealyAutomaton
    { mealyDelta :: state -> input -> state
    , mealyLambda :: state -> input -> output
    , mealyInitialS :: state
    , mealyCurrentS :: state
    }

mkMealyAutomaton :: (s -> i -> s) -> (s -> i -> o) -> s -> MealyAutomaton i o s
mkMealyAutomaton delta lambda initial =
    MealyAutomaton
        { mealyDelta = delta
        , mealyLambda = lambda
        , mealyInitialS = initial
        , mealyCurrentS = initial
        }

mkMealyAutomaton2 :: (s -> i -> (s, o)) -> s -> MealyAutomaton i o s
mkMealyAutomaton2 transitions initial =
    MealyAutomaton
        { mealyDelta = \s i -> fst (transitions s i)
        , mealyLambda = \s i -> snd (transitions s i)
        , mealyInitialS = initial
        , mealyCurrentS = initial
        }

mealyUpdateState :: MealyAutomaton i o s -> s -> MealyAutomaton i o s
mealyUpdateState m s =
    MealyAutomaton
        { mealyDelta = delta
        , mealyLambda = lambda
        , mealyInitialS = initial
        , mealyCurrentS = s
        }
  where
    delta = mealyDelta m
    lambda = mealyLambda m
    initial = mealyInitialS m

mealyStep :: MealyAutomaton i o s -> i -> (MealyAutomaton i o s, o)
mealyStep m i = (mealyUpdateState m nextState, output)
  where
    nextState = mealyDelta m (mealyCurrentS m) i
    output = mealyLambda m (mealyCurrentS m) i

mealyWalk :: (Traversable t) => MealyAutomaton i o s -> t i -> (MealyAutomaton i o s, t o)
mealyWalk = List.mapAccumL mealyStep

mealyReset :: MealyAutomaton i o s -> MealyAutomaton i o s
mealyReset m = mealyUpdateState m (mealyInitialS m)

mealyTransitions ::
    forall i o s.
    (Data.Data s, Data.Data i, Ord s, Ord i) =>
    MealyAutomaton i o s ->
    Map.Map (s, i) (s, o)
mealyTransitions m = Map.fromList [((s, i), (delta s i, lambda s i)) | s <- domainS, i <- domainI]
  where
    delta = mealyDelta m
    lambda = mealyLambda m
    constructorsS = Data.dataTypeConstrs $ Data.dataTypeOf (undefined :: s)
    constructorsI = Data.dataTypeConstrs $ Data.dataTypeOf (undefined :: i)
    domainS = List.map Data.fromConstr constructorsS
    domainI = List.map Data.fromConstr constructorsI

instance BlackBox.BlackBox MealyAutomaton where
    step = mealyStep
    walk = mealyWalk
    current = mealyCurrentS

instance BlackBox.Automaton MealyAutomaton where
    transitions = mealyTransitions

instance BlackBox.SUL MealyAutomaton where
    reset = mealyReset
