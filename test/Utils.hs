{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils (
    findReachable,
    statesAreEquivalent,
    NonMinimalMealy (..),
    Mealy (..),
    Input (..),
    Output (..),
    State (..),
    ArbWMethod (..),
)
where

import Automaton.MealyAutomaton (
    MealyAutomaton (..),
    mealyDelta,
    mealyInAlphabet,
    mealyLambda,
    mealyOutAlphabet,
    mealyStates,
    mealyStep,
    mealyUpdateState,
 )

import qualified Data.Bifunctor as Bif
import Data.Data (Data)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, vectorOf)

import qualified Data.Set as Set
import EquivalenceOracle.WMethod (WMethod (..))

newtype ArbWMethod = ArbWMethod WMethod deriving (Show, Eq)

instance Arbitrary ArbWMethod where
    arbitrary = do
        d <- choose (0, 5)
        return (ArbWMethod (WMethod d))

newtype Mealy s i o = Mealy (MealyAutomaton s i o) deriving (Show)

instance
    ( Arbitrary i
    , Arbitrary o
    , Arbitrary s
    , Ord i
    , Enum i
    , Bounded i
    , Ord o
    , Enum o
    , Bounded o
    , Ord s
    , Enum s
    , Bounded s
    ) =>
    Arbitrary (Mealy s i o)
    where
    arbitrary = do
        let states = [minBound .. maxBound]
        delta <- generateDelta states
        lambda <- generateLambda states

        initialState <- arbitrary
        currentState <- arbitrary

        return
            ( Mealy
                ( MealyAutomaton
                    { mealyDelta = delta
                    , mealyLambda = lambda
                    , mealyInitialS = initialState
                    , mealyCurrentS = currentState
                    , mealyStates = Set.fromList states
                    }
                )
            )
      where
        generateDelta :: [s] -> Gen (s -> i -> s)
        generateDelta states = do
            let
                inputs = Set.toList $ mealyInAlphabet (undefined :: MealyAutomaton s i o)
                complete = [(st, inp) | st <- states, inp <- inputs]
                (numS, numI) = Bif.bimap List.length List.length (states, inputs)
            matching <- vectorOf (numS * numI) (choose (0, numS - 1))
            let stateOutputs = [states !! index | index <- matching]
                stateMappings = Map.fromList $ List.zip complete stateOutputs
            fallbackState <- arbitrary :: Gen s
            return $ \s i -> Data.Maybe.fromMaybe fallbackState (Map.lookup (s, i) stateMappings)

        generateLambda :: [s] -> Gen (s -> i -> o)
        generateLambda states = do
            let
                inputs = Set.toList $ mealyInAlphabet (undefined :: MealyAutomaton s i o)
                outputs = Set.toList $ mealyOutAlphabet (undefined :: MealyAutomaton s i o)
                complete = [(st, inp) | st <- states, inp <- inputs]
                (numS, numI) = Bif.bimap List.length List.length (states, inputs)
                numO = List.length outputs
            matching <- vectorOf (numS * numI) (choose (0, numO - 1))
            let outputOutputs = [outputs !! index | index <- matching]
                outputMappings = Map.fromList $ List.zip complete outputOutputs
            fallbackOutput <- arbitrary :: Gen o
            return $ \s i -> Data.Maybe.fromMaybe fallbackOutput (Map.lookup (s, i) outputMappings)

data Input = A | B | C | D deriving (Show, Eq, Ord, Data, Enum, Bounded)
data Output = X | Y | Z | W deriving (Show, Eq, Ord, Data, Enum, Bounded)
data State = S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 deriving (Show, Eq, Ord, Data, Enum, Bounded)

-- Arbitrary instances for Input, Output, and State
instance Arbitrary Input where
    arbitrary = elements [A, B, C, D]

instance Arbitrary Output where
    arbitrary = elements [X, Y, Z, W]

instance Arbitrary State where
    arbitrary = elements [S0, S1, S2, S3, S4, S5, S6, S7]

newtype NonMinimalMealy = NonMinimalMealy (MealyAutomaton State Input Output) deriving (Show)

instance Arbitrary NonMinimalMealy where
    arbitrary = do
        let states = [minBound .. maxBound]
        delta <- generateDelta states
        lambda <- generateLambda states

        initialState <- arbitrary :: Gen State
        currentState <- arbitrary :: Gen State

        return
            ( NonMinimalMealy
                ( MealyAutomaton
                    { mealyDelta = delta
                    , mealyLambda = lambda
                    , mealyInitialS = initialState
                    , mealyCurrentS = currentState
                    , mealyStates = Set.fromList states
                    }
                )
            )
      where
        generateDelta :: [State] -> Gen (State -> Input -> State)
        generateDelta states = do
            let
                inputs = Set.toList $ mealyInAlphabet (undefined :: MealyAutomaton State Input Output)
                (numS, numI) = Bif.bimap List.length List.length (states, inputs)
                same = numS `div` 2
                nonMinimal = [(st, inp) | st <- take same states, inp <- inputs]
                rest = [(st, inp) | st <- drop same states, inp <- inputs]
            nonMinimalMatching1 <- vectorOf numI (choose (0, numS - 1))
            nonMinimalMatching2 <- vectorOf ((numS - same) * numI) (choose (0, numS - 1))
            let stateOutputs1 = [states !! index | index <- concat (replicate same nonMinimalMatching1)]
                stateOutputs2 = [states !! index | index <- nonMinimalMatching2]
                nonMinimalMappings = Map.fromList $ List.zip (nonMinimal ++ rest) (stateOutputs1 ++ stateOutputs2)
            fallbackState <- arbitrary :: Gen State
            return $ \s i -> Data.Maybe.fromMaybe fallbackState (Map.lookup (s, i) nonMinimalMappings)

        generateLambda :: [State] -> Gen (State -> Input -> Output)
        generateLambda states = do
            let
                inputs = Set.toList $ mealyInAlphabet (undefined :: MealyAutomaton State Input Output)
                outputs = Set.toList $ mealyOutAlphabet (undefined :: MealyAutomaton State Input Output)
                same = numS `div` 2
                nonMinimal = [(st, inp) | st <- take same states, inp <- inputs]
                rest = [(st, inp) | st <- drop same states, inp <- inputs]
                (numS, numI) = Bif.bimap List.length List.length (states, inputs)
                numO = List.length outputs
            nonMinimalMatching1 <- vectorOf numI (choose (0, numO - 1))
            nonMinimalMatching2 <- vectorOf ((numS - same) * numI) (choose (0, numO - 1))
            let outputOutputs1 = [outputs !! index | index <- concat (replicate same nonMinimalMatching1)]
                outputOutputs2 = [outputs !! index | index <- nonMinimalMatching2]
                outputMappings = Map.fromList $ List.zip (nonMinimal ++ rest) (outputOutputs1 ++ outputOutputs2)
            fallbackOutput <- arbitrary :: Gen Output
            return $ \s i -> Data.Maybe.fromMaybe fallbackOutput (Map.lookup (s, i) outputMappings)

-- Two states are equivalent if their delta and lambda functions are equivalent.
statesAreEquivalent :: MealyAutomaton State Input Output -> State -> State -> Bool
statesAreEquivalent _ s1 s2 | s1 == s2 = True
statesAreEquivalent automaton s1 s2 =
    all (\i -> (delta s1 i, lambda s1 i) == (delta s2 i, lambda s2 i)) alphabet
  where
    delta = mealyDelta automaton
    lambda = mealyLambda automaton
    alphabet = mealyInAlphabet automaton

-- Starts a bfs from the initial state and finds all reachable states
findReachable :: MealyAutomaton State Input Output -> Set.Set State
findReachable automaton =
    let initialState = mealyInitialS automaton
        alphabet = mealyInAlphabet automaton
        bfs visited [] = visited
        bfs visited (current : queue) =
            let mo = mealyUpdateState automaton current
                nextStates = Set.map (mealyCurrentS . fst . mealyStep mo) alphabet
                newVisited = Set.foldr Set.insert visited nextStates
                newQueue = queue ++ [s | s <- Set.toList nextStates, s `notElem` visited]
             in bfs newVisited newQueue
     in bfs (Set.singleton initialState) [initialState]
