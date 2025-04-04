{-# LANGUAGE DeriveDataTypeable #-}

module MealyAutomatonSpec (
    spec,
)
where

import qualified Data.Bifunctor as Bif
import Data.Data (Data)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import MealyAutomaton (
    MealyAutomaton (..),
    mealyDelta,
    mealyGlobalCharacterizingSet,
    mealyInAlphabet,
    mealyLambda,
    mealyOutAlphabet,
    mealyStates,
    mealyTransitions,
 )
import Test.Hspec (Spec, context, describe, it)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, elements, property, vectorOf, (==>))

data Input = A | B | C | D deriving (Show, Eq, Ord, Data)
data Output = X | Y | Z | W deriving (Show, Eq, Ord, Data)
data State = S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 deriving (Show, Eq, Ord, Data)

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

-- Property to test if two states with the same delta and lambda are equivalent
statesAreEquivalent :: NonMinimalMealy -> State -> State -> Bool
statesAreEquivalent (NonMinimalMealy automaton) s1 s2 =
    all
        ( \i ->
            mealyDelta automaton s1 i == mealyDelta automaton s2 i
                && mealyLambda automaton s1 i == mealyLambda automaton s2 i
        )
        (mealyInAlphabet automaton)

-- Check if the empty list [] is an element of the characterizing set
emptyListInCharacterizingSet :: NonMinimalMealy -> State -> State -> Property
emptyListInCharacterizingSet (NonMinimalMealy automaton) s1 s2 =
    statesAreEquivalent (NonMinimalMealy automaton) s1 s2
        && s1
            /= s2
                ==> []
                `Set.member` mealyGlobalCharacterizingSet automaton

mappingEquivalentToFunctions :: MealyAutomaton Input Output State -> Bool
mappingEquivalentToFunctions automaton =
    let transitions = mealyTransitions automaton
        alphabet = Set.toList $ mealyInAlphabet automaton
        states = Set.toList $ mealyStates automaton
        -- Calculate outputs using mealyDelta and mealyLambda
        mapOutputs =
            [ Maybe.fromJust (Map.lookup (s, a) transitions)
            | s <- states
            , a <- alphabet
            ]
        funOutputs = [(mealyDelta automaton s a, mealyLambda automaton s a) | s <- states, a <- alphabet]
     in mapOutputs == funOutputs

spec :: Spec
spec = do
    describe "MealyAutomaton.mealyGlobalCharacterizingSet" $
        context "if the automaton contains at least 2 equivalent states" $
            it "returns a set that contains the empty list" $
                property
                    emptyListInCharacterizingSet

    describe "MealyAutomaton.mealyTransitions" $
        it "returns a map equivalent to the transition and output functions of the model" $
            property
                mappingEquivalentToFunctions
