{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module MealyAutomaton
  ( MealyAutomaton,
    MealyError (..),
    Result (..),
    mkMealyAutomaton,
    mealyStep,
    mealyWalk,
    mealyStateTransitions,
    mealyReset,
  )
where

import Data.Data
import qualified Data.List as List
import qualified Data.Map as Map
import qualified BlackBox

data MealyAutomaton input output state = MealyAutomaton
  { transitions :: Map.Map (state, input) (state, output),
    current :: state
  }
  deriving (Show, Eq)

data MealyError = UnusedInputSymbol | UndefinedTransitions deriving (Show, Eq)

data Result value e = Value value | Error e deriving (Show, Eq)

-- | Create a Mealy Automaton from a map of transitions and a starting state.
--     The transitions map should contain all possible transitions for the automaton.
--     The starting state should be a valid state in the transitions map.
--     The function returns a MealyAutomaton if the transitions are complete and the input alphabet is used.
--     Otherwise, it returns an error.
mkMealyAutomaton ::
  forall i o s.
  (Data i, Eq s, Eq i) =>
  -- | The transitions of the automaton
  Map.Map (s, i) (s, o) ->
  -- | The starting state of the automaton
  s ->
  -- | The Mealy Automaton or an error
  Result (MealyAutomaton i o s) MealyError
mkMealyAutomaton transitions state =
  let -- verify that all of the input alphabet is used
      inputAlphabet = dataTypeConstrs $ dataTypeOf (Proxy :: Proxy i)
      keys = Map.keys transitions
      froms = List.map (toConstr . snd) keys
      allUsed = List.all (`List.elem` froms) inputAlphabet

      -- verify that the transitions of the states are defined for each input
      states = List.nub $ List.map fst keys
      statesComplete = List.all (`List.elem` keys) [(s, fromConstr i) | s <- states, i <- inputAlphabet]
   in if allUsed
        then
          if statesComplete
            then Value MealyAutomaton {transitions = transitions, current = state}
            else Error UndefinedTransitions
        else Error UnusedInputSymbol

-- | Step the Mealy Automaton with an input symbol.
mealyStep :: (Ord i, Ord s) => MealyAutomaton i o s -> i -> (MealyAutomaton i o s, o)
mealyStep before input = (MealyAutomaton {transitions = edges, current = nextState}, output)
  where
    edges = transitions before
    currState = current before
    (nextState, output) = case Map.lookup (currState, input) edges of
      Just (nextState, output) -> (nextState, output)
      Nothing -> error "A transition for some state was not found."

mealyWalk :: (Ord i, Ord s) => MealyAutomaton i o s -> [i] -> (MealyAutomaton i o s, [o])
mealyWalk before inputs = (MealyAutomaton {transitions = edges, current = nextState}, outputs)
  where
    edges = transitions before
    help m [] sofar = (current m, List.reverse sofar)
    help m (i : is) sofar =
      let (nextModel, nextO) = mealyStep m i
       in help nextModel is (nextO : sofar)
    (nextState, outputs) = help before inputs []

mealyStateTransitions :: (Eq s) => MealyAutomaton i o s -> s -> Map.Map (s, i) (s, o)
mealyStateTransitions mo st = Map.filterWithKey (\k _ -> fst k == st) $ transitions mo

mealyReset :: forall i o s. (Bounded s) => MealyAutomaton i o s -> MealyAutomaton i o s
mealyReset MealyAutomaton {transitions = transitions, current = _} =
  MealyAutomaton {transitions = transitions, current = minBound :: s}

instance BlackBox.BlackBox MealyAutomaton where
  step = mealyStep
  walk = mealyWalk
  current = current

instance BlackBox.Automaton MealyAutomaton where
  transitions = transitions

instance BlackBox.SUL MealyAutomaton where
  reset = mealyReset
