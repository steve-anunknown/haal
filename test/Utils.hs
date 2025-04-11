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

import MealyAutomaton (
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
import WMethod (WMethod (..))

newtype ArbWMethod = ArbWMethod WMethod deriving (Show, Eq)

instance Arbitrary ArbWMethod where
    arbitrary = do
        d <- choose (0, 5)
        return (ArbWMethod (WMethod d))

newtype Mealy i o s = Mealy (MealyAutomaton i o s) deriving (Show)

instance
    ( Arbitrary i
    , Arbitrary o
    , Arbitrary s
    , Ord s
    , Ord i
    , Ord o
    , Data s
    , Data i
    , Data o
    ) =>
    Arbitrary (Mealy i o s)
    where
    arbitrary = do
        delta <- generateDelta
        lambda <- generateLambda

        initialState <- arbitrary
        currentState <- arbitrary

        return
            ( Mealy
                ( MealyAutomaton
                    { mealyDelta = delta
                    , mealyLambda = lambda
                    , mealyInitialS = initialState
                    , mealyCurrentS = currentState
                    }
                )
            )
      where
        generateDelta :: Gen (s -> i -> s)
        generateDelta = do
            let states = Set.toList $ mealyStates (undefined :: MealyAutomaton i o s)
                inputs = Set.toList $ mealyInAlphabet (undefined :: MealyAutomaton i o s)
                complete = [(st, inp) | st <- states, inp <- inputs]
                (numS, numI) = Bif.bimap List.length List.length (states, inputs)
            matching <- vectorOf (numS * numI) (choose (0, numS - 1))
            let stateOutputs = [states !! index | index <- matching]
                stateMappings = Map.fromList $ List.zip complete stateOutputs
            fallbackState <- arbitrary :: Gen s
            return $ \s i -> Data.Maybe.fromMaybe fallbackState (Map.lookup (s, i) stateMappings)

        generateLambda :: Gen (s -> i -> o)
        generateLambda = do
            let states = Set.toList $ mealyStates (undefined :: MealyAutomaton i o s)
                inputs = Set.toList $ mealyInAlphabet (undefined :: MealyAutomaton i o s)
                outputs = Set.toList $ mealyOutAlphabet (undefined :: MealyAutomaton i o s)
                complete = [(st, inp) | st <- states, inp <- inputs]
                (numS, numI) = Bif.bimap List.length List.length (states, inputs)
                numO = List.length outputs
            matching <- vectorOf (numS * numI) (choose (0, numO - 1))
            let outputOutputs = [outputs !! index | index <- matching]
                outputMappings = Map.fromList $ List.zip complete outputOutputs
            fallbackOutput <- arbitrary :: Gen o
            return $ \s i -> Data.Maybe.fromMaybe fallbackOutput (Map.lookup (s, i) outputMappings)

data Input = A | B | C | D deriving (Show, Eq, Ord, Data)
data Output = X | Y | Z | W deriving (Show, Eq, Ord, Data)
data State = S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 deriving (Show, Eq, Ord, Data, Bounded)

-- Arbitrary instances for Input, Output, and State
instance Arbitrary Input where
    arbitrary = elements [A, B, C, D]

instance Arbitrary Output where
    arbitrary = elements [X, Y, Z, W]

instance Arbitrary State where
    arbitrary = elements [S0, S1, S2, S3, S4, S5, S6, S7]

newtype NonMinimalMealy = NonMinimalMealy (MealyAutomaton Input Output State) deriving (Show)

instance Arbitrary NonMinimalMealy where
    arbitrary = do
        delta <- generateDelta
        lambda <- generateLambda

        initialState <- arbitrary :: Gen State
        currentState <- arbitrary :: Gen State

        return
            ( NonMinimalMealy
                ( MealyAutomaton
                    { mealyDelta = delta
                    , mealyLambda = lambda
                    , mealyInitialS = initialState
                    , mealyCurrentS = currentState
                    }
                )
            )
      where
        generateDelta :: Gen (State -> Input -> State)
        generateDelta = do
            let states = Set.toList $ mealyStates (undefined :: MealyAutomaton Input Output State)
                inputs = Set.toList $ mealyInAlphabet (undefined :: MealyAutomaton Input Output State)
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

        generateLambda :: Gen (State -> Input -> Output)
        generateLambda = do
            let states = Set.toList $ mealyStates (undefined :: MealyAutomaton Input Output State)
                inputs = Set.toList $ mealyInAlphabet (undefined :: MealyAutomaton Input Output State)
                outputs = Set.toList $ mealyOutAlphabet (undefined :: MealyAutomaton Input Output State)
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
statesAreEquivalent :: MealyAutomaton Input Output State -> State -> State -> Bool
statesAreEquivalent _ s1 s2 | s1 == s2 = True
statesAreEquivalent automaton s1 s2 | otherwise =
    all ( \i -> (delta s1 i, lambda s1 i) == (delta s2 i, lambda s2 i) ) alphabet
        where delta = mealyDelta automaton
              lambda = mealyLambda automaton
              alphabet = mealyInAlphabet automaton

-- Starts a bfs from the initial state and finds all reachable states
findReachable :: MealyAutomaton Input Output State -> Set.Set State
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
