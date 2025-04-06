{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module tests the Mealy automaton implementation.
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
    mealyAccessSequences,
    mealyDelta,
    mealyDistinguishingSequence,
    mealyGlobalCharacterizingSet,
    mealyInAlphabet,
    mealyLambda,
    mealyOutAlphabet,
    mealyStates,
    mealyStep,
    mealyTransitions,
    mealyUpdateState,
    mealyWalk,
 )
import Test.Hspec (Spec, context, describe, it)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, elements, property, vectorOf, (==>))

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

-- Two states are equivalent if their delta and lambda functions are equivalent.
statesAreEquivalent :: MealyAutomaton Input Output State -> State -> State -> Bool
statesAreEquivalent automaton s1 s2 =
    all
        ( \i ->
            mealyDelta automaton s1 i == mealyDelta automaton s2 i
                && mealyLambda automaton s1 i == mealyLambda automaton s2 i
        )
        (mealyInAlphabet automaton)

-- The global characterizing set of a non minimal mealy automaton contains
-- the empty list.
prop_emptyListInCharacterizingSet :: NonMinimalMealy -> State -> State -> Property
prop_emptyListInCharacterizingSet (NonMinimalMealy automaton) s1 s2 =
    statesAreEquivalent automaton s1 s2
        && s1
            /= s2
                ==> []
                `Set.member` mealyGlobalCharacterizingSet automaton

-- Two states that are not equivalent can be distinguished.
prop_existsDistinguishingSequence :: Mealy Input Output State -> State -> State -> Property
prop_existsDistinguishingSequence (Mealy automaton) s1 s2 =
    not (statesAreEquivalent automaton s1 s2)
        ==> output1
        /= output2
        && output1 /= []
        && output2 /= []
  where
    dist = mealyDistinguishingSequence automaton s1 s2
    (_, output1) = mealyWalk (mealyUpdateState automaton s1) dist
    (_, output2) = mealyWalk (mealyUpdateState automaton s2) dist

-- The map returned by 'mealyTransitions' is equivalent to the 'mealyLambda'
-- and 'mealyDelta' functions of the automaton.
prop_mappingEquivalentToFunctions :: Mealy Input Output State -> Bool
prop_mappingEquivalentToFunctions (Mealy automaton) =
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

-- The access sequences returned by 'mealyAccessSequences' cover all reachable states.
prop_completeAccessSequences :: Mealy Input Output State -> Property
prop_completeAccessSequences (Mealy automaton) = states == reachable ==> allin
  where
    seqs = mealyAccessSequences automaton
    states = mealyStates automaton
    reachable = findReachable automaton
    allin = all (`Map.member` seqs) reachable

-- The access sequences returned by 'mealyAccessSequences' are the shortest
prop_shortestAccessSequences :: Mealy Input Output State -> State -> State -> Property
prop_shortestAccessSequences (Mealy automaton) s1 s2 =
    s1 `Set.member` reachable
        && s2 `Set.member` reachable
        && existsS1toS2 ==> List.length seq2 <= List.length seq1 + 1
  where
    reachable = findReachable automaton
    transitions = mealyTransitions automaton
    accessSeqs = mealyAccessSequences automaton
    seq1 = accessSeqs Map.! s1
    seq2 = accessSeqs Map.! s2
    -- find transition in map (s, i) -> (s, o)
    -- that leads from s1 to s2
    listed = Map.toList transitions
    filtering (s, i) = s == s1 && fst (transitions Map.! (s, i)) == s2
    maybeTransition = List.find filtering $ List.map fst listed
    existsS1toS2 = case maybeTransition of
        Nothing -> False
        Just _ -> True

spec :: Spec
spec = do
    describe "MealyAutomaton.mealyDistinguishingSequence" $
        context "if 2 automatons states are not equivalent" $
            it "returns an input sequence that distinguishes them" $
                property
                    prop_existsDistinguishingSequence

    describe "MealyAutomaton.mealyGlobalCharacterizingSet" $
        context "if the automaton contains at least 2 equivalent states" $
            it "returns a set that contains the empty list" $
                property
                    prop_emptyListInCharacterizingSet

    describe "MealyAutomaton.mealyTransitions" $
        it "returns a map equivalent to the transition and output functions of the model" $
            property
                prop_mappingEquivalentToFunctions

    describe "MealyAutomaton.mealyAccessSequences" $ do
        it "returns a map from states to list of inputs that covers all reachable states" $
            property
                prop_completeAccessSequences

        it "returns a map from reachable states to shortest list of inputs that access them" $
            property
                prop_shortestAccessSequences
