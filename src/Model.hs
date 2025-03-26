module Model
  ( Model (..),
    mkModel,
    modelStep,
    modelTrace,
  )
where

import Data.List as List (all, map)
import qualified Data.List as List
import Data.Map as Map
import Data.Set as Set
import GHC.Natural

type Symbol = String

type State = Natural

data Model = Model
  { inputAlphabet :: Set.Set Symbol,
    outputAlphabet :: Set.Set Symbol,
    transitions :: Map.Map (State, Symbol) (State, Symbol),
    currentState :: Natural
  }
  deriving (Show)

validateInputs :: Set.Set Symbol -> Map.Map (State, Symbol) (State, Symbol) -> Bool
validateInputs inputs edges =
  let fromPairs = Map.keys edges
      providedInputs = List.map snd fromPairs
   in List.all (`Set.member` inputs) providedInputs

mkModel :: Set.Set Symbol -> Set.Set Symbol -> Map.Map (State, Symbol) (State, Symbol) -> Maybe Model
mkModel inputs outputs edges =
  if validateInputs inputs edges
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
mkModelLike Model {inputAlphabet = ins, outputAlphabet = outs, transitions = trans} state =
  Model
    { inputAlphabet = ins,
      outputAlphabet = outs,
      transitions = trans,
      currentState = state
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
