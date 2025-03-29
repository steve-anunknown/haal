{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MealyAutomatonFormal (
    MealyAutomatonFormal (..),
    mkMealyAutomatonFormal,
    mkMealyAutomatonFormal2,
    mealyStep,
    mealyWalk,
    mealyReset,
)
where

import qualified BlackBox
import qualified Data.Data as Data
import qualified Data.List as List
import qualified Data.Map as Map

data MealyAutomatonFormal input output state = MealyAutomatonFormal
    { mealyDelta :: state -> input -> state
    , mealyLambda :: state -> input -> output
    , mealyInitialS :: state
    , mealyCurrentS :: state
    }

mkMealyAutomatonFormal :: (s -> i -> s) -> (s -> i -> o) -> s -> MealyAutomatonFormal i o s
mkMealyAutomatonFormal delta lambda initial =
    MealyAutomatonFormal
        { mealyDelta = delta
        , mealyLambda = lambda
        , mealyInitialS = initial
        , mealyCurrentS = initial
        }

mkMealyAutomatonFormal2 :: (s -> i -> (s, o)) -> s -> MealyAutomatonFormal i o s
mkMealyAutomatonFormal2 transitions initial =
    MealyAutomatonFormal
        { mealyDelta = \s i -> fst (transitions s i)
        , mealyLambda = \s i -> snd (transitions s i)
        , mealyInitialS = initial
        , mealyCurrentS = initial
        }

mealyUpdateState :: MealyAutomatonFormal i o s -> s -> MealyAutomatonFormal i o s
mealyUpdateState m s =
    MealyAutomatonFormal
        { mealyDelta = delta
        , mealyLambda = lambda
        , mealyInitialS = initial
        , mealyCurrentS = s
        }
  where
    delta = mealyDelta m
    lambda = mealyLambda m
    initial = mealyInitialS m

mealyStep :: MealyAutomatonFormal i o s -> i -> (MealyAutomatonFormal i o s, o)
mealyStep m i = (mealyUpdateState m nextState, output)
  where
    nextState = mealyDelta m (mealyCurrentS m) i
    output = mealyLambda m (mealyCurrentS m) i

mealyWalk :: (Traversable t) => MealyAutomatonFormal i o s -> t i -> (MealyAutomatonFormal i o s, t o)
mealyWalk = List.mapAccumL mealyStep

mealyReset :: MealyAutomatonFormal i o s -> MealyAutomatonFormal i o s
mealyReset m = mealyUpdateState m (mealyInitialS m)

mealyTransitions ::
    forall i o s.
    (Data.Data s, Data.Data i, Ord s, Ord i) =>
    MealyAutomatonFormal i o s ->
    Map.Map (s, i) (s, o)
mealyTransitions m = Map.fromList [((s, i), (delta s i, lambda s i)) | s <- domainS, i <- domainI]
  where
    delta = mealyDelta m
    lambda = mealyLambda m
    constructorsS = Data.dataTypeConstrs $ Data.dataTypeOf (undefined :: s)
    constructorsI = Data.dataTypeConstrs $ Data.dataTypeOf (undefined :: i)
    domainS = List.map Data.fromConstr constructorsS :: [s]
    domainI = List.map Data.fromConstr constructorsI :: [i]

instance BlackBox.BlackBox MealyAutomatonFormal where
    step = mealyStep
    walk = mealyWalk
    current = mealyCurrentS

instance BlackBox.Automaton MealyAutomatonFormal where
    transitions = mealyTransitions

instance BlackBox.SUL MealyAutomatonFormal where
    reset = mealyReset

data Input = A | B | C deriving (Show, Eq, Ord, Data.Data)

data Output = D | E | F deriving (Show, Eq, Ord, Data.Data)

data State = S0 | S1 | S2 deriving (Show, Eq, Ord, Data.Data)

myFunction :: State -> Input -> (State, Output)
myFunction S0 A = (S1, D)
myFunction S0 B = (S1, E)
myFunction S0 C = (S1, F)
myFunction S1 A = (S2, D)
myFunction S1 B = (S2, E)
myFunction S1 C = (S2, F)
myFunction S2 A = (S0, D)
myFunction S2 B = (S0, E)
myFunction S2 C = (S0, F)

myModel :: MealyAutomatonFormal Input Output State
myModel = mkMealyAutomatonFormal2 myFunction S0
