{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements a Mealy automaton.
module MealyAutomaton (
    MealyAutomaton (..),
    mkMealyAutomaton,
    mkMealyAutomaton2,
    mealyStep,
    mealyWalk,
    mealyReset,
    mealyDistinguishingSequence,
)
where

import qualified BlackBox
import qualified Data.Data as Data
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

{- | The 'MealyAutomaton' data type is parameterised by the 'input', 'output' and 'state' types
 which play the role of the input alphabet, output alphabet and set of states respectively.
 The transitions of the automaton are defined by the 'mealyDelta' and 'mealyLambda' functions,
 which respectively return the new state after a transition and the produced output. Finally,
 the 'mealyInitialS' defines the initial state of the automaton and the 'mealyCurrentS' defines
 the current state which the automaton is in.
-}
data MealyAutomaton input output state = MealyAutomaton
    { mealyDelta :: state -> input -> state
    , mealyLambda :: state -> input -> output
    , mealyInitialS :: state
    , mealyCurrentS :: state
    }

{- | The 'mkMealyAutomaton' constructor returns a 'MealyAutomaton' by requiring the 'mealyDelta'
function, the 'mealyLambda' function and the initial state 'mealyInitialS'.
-}
mkMealyAutomaton :: (s -> i -> s) -> (s -> i -> o) -> s -> MealyAutomaton i o s
mkMealyAutomaton delta lambda initial =
    MealyAutomaton
        { mealyDelta = delta
        , mealyLambda = lambda
        , mealyInitialS = initial
        , mealyCurrentS = initial
        }

{- | The 'mkMealyAutomaton2' constructor returns a 'MealyAutomaton' by requiring just one
function describing both the state transitions as well as the produced outputs, instead of two
separate functions, and the initial state 'mealyInitialS'.
-}
mkMealyAutomaton2 :: (s -> i -> (s, o)) -> s -> MealyAutomaton i o s
mkMealyAutomaton2 transitions initial =
    MealyAutomaton
        { mealyDelta = \s i -> fst (transitions s i)
        , mealyLambda = \s i -> snd (transitions s i)
        , mealyInitialS = initial
        , mealyCurrentS = initial
        }

-- | Convenience function that updates the current state 'mealyCurrentS' of the automaton.
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

{- | Performs a step in the automaton and returns a tuple containing the automaton with a modified
state as well as the output produced by the transition.
-}
mealyStep :: MealyAutomaton i o s -> i -> (MealyAutomaton i o s, o)
mealyStep m i = (mealyUpdateState m nextState, output)
  where
    nextState = mealyDelta m (mealyCurrentS m) i
    output = mealyLambda m (mealyCurrentS m) i

{- | Extends the 'mealyStep' function by taking a series of inputs and performing multiple steps in
the automaton, returning a tuple containing the automaton with a modified state as well as the
outputs produced by the transitions.
-}
mealyWalk :: (Traversable t) => MealyAutomaton i o s -> t i -> (MealyAutomaton i o s, t o)
mealyWalk = List.mapAccumL mealyStep

-- | Resets the automaton to its initial state.
mealyReset :: MealyAutomaton i o s -> MealyAutomaton i o s
mealyReset m = mealyUpdateState m (mealyInitialS m)

{- | Returns a map describing the combined behaviour of the 'mealyDelta'
and 'mealyLambda' functions.
-}
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

mealyAlphabet ::
    forall i o s.
    (Ord i, Data.Data i) =>
    MealyAutomaton i o s ->
    Set.Set i
mealyAlphabet _ = Set.fromList (List.map Data.fromConstr (Data.dataTypeConstrs $ Data.dataTypeOf (undefined :: i)) :: [i])

mealyCharacterizingSet :: MealyAutomaton i o s -> Set.Set [i]
mealyCharacterizingSet m = undefined

mealyDistinguishingSequence ::
    forall i o s.
    (Ord i, Data.Data i, Ord s, Ord o) =>
    MealyAutomaton i o s ->
    s ->
    s ->
    [i]
mealyDistinguishingSequence _ s1 s2 | s1 == s2 = []
mealyDistinguishingSequence m s1 s2 = explore [(model1, model2, [])]
  where
    model1 = mealyUpdateState m s1
    model2 = mealyUpdateState m s2
    alphabet = Set.toList (mealyAlphabet m)

    explore :: [(MealyAutomaton i o s, MealyAutomaton i o s, [i])] -> [i]
    explore [] = []
    explore (x : xs) = result
      where
        (m1, m2, prefix) = x
        (models1, outputs1) = List.unzip $ List.map (mealyStep m1) alphabet
        (models2, outputs2) = List.unzip $ List.map (mealyStep m2) alphabet
        discrepancy = List.elemIndex False $ List.zipWith (/=) outputs1 outputs2
        result = case discrepancy of
            Just index -> List.reverse (alphabet !! index : prefix)
            Nothing -> explore (xs ++ List.zip3 models1 models2 [i : prefix | i <- alphabet])

instance BlackBox.BlackBox MealyAutomaton where
    step = mealyStep
    walk = mealyWalk
    current = mealyCurrentS
    alphabet = mealyAlphabet

instance BlackBox.Automaton MealyAutomaton where
    transitions = mealyTransitions

instance BlackBox.SUL MealyAutomaton where
    reset = mealyReset
