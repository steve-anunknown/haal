{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Model
  ( Model (..),
    mkModel,
    modelStep,
    modelTrace,
    stateTransitions,
  )
where

import Automaton
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Natural

data Model = Model
  { inputAlphabet :: Set.Set Symbol,
    outputAlphabet :: Set.Set Symbol,
    transitions :: Map.Map (State, Symbol) (State, Symbol),
    currentState :: State
  }
  deriving (Show)

validateInputs :: Set.Set Symbol -> Map.Map (State, Symbol) (State, Symbol) -> Bool
validateInputs inputs edges =
  let fromPairs = Map.keys edges
      providedInputs = List.map snd fromPairs
   in List.all (`Set.member` inputs) providedInputs

validateComplete :: Set.Set Symbol -> Map.Map (State, Symbol) (State, Symbol) -> Bool
validateComplete inputs edges =
  let froms = Map.keys edges
      states = List.sort $ List.nub $ List.map fst froms
      areConsecutive = states == [(head states) .. states !! (length states - 1)]
      areComplete = List.all (`List.elem` froms) [(s, i) | s <- states, i <- Set.toList inputs]
   in areConsecutive && areComplete

mkModel :: Set.Set Symbol -> Set.Set Symbol -> Map.Map (State, Symbol) (State, Symbol) -> Maybe Model
mkModel inputs outputs edges =
  if validateInputs inputs edges && validateComplete inputs edges
    then
      Just $
        Model
          { inputAlphabet = inputs,
            outputAlphabet = outputs,
            transitions = edges,
            currentState = 0
          }
    else Nothing

mkModelLike :: Model -> Natural -> Model
mkModelLike Model {inputAlphabet = ins, outputAlphabet = outs, transitions = trans} current =
  Model
    { inputAlphabet = ins,
      outputAlphabet = outs,
      transitions = trans,
      currentState = current
    }

modelStep :: Model -> Symbol -> Maybe (Model, Symbol)
modelStep model@Model {currentState = cur, transitions = edges} input =
  case Map.lookup (cur, input) edges of
    Just (nextS, output) -> Just (mkModelLike model nextS, output)
    Nothing -> Nothing

modelTrace :: Model -> [Symbol] -> Maybe (Model, [Symbol])
modelTrace model inputs = help model inputs []
  where
    help m [] sofar = Just (m, List.reverse sofar)
    help m (i : is) sofar = case modelStep m i of
      Just (nextM, o) -> help nextM is (o : sofar)
      Nothing -> Nothing

stateTransitions :: Model -> State -> Map.Map (State, Symbol) (State, Symbol)
stateTransitions m s = Map.filterWithKey (\k _ -> fst k == s) $ transitions m

instance Automaton Model where
  step = modelStep
  walk = modelTrace
  state = currentState
  inputs = inputAlphabet
  outputs = outputAlphabet
