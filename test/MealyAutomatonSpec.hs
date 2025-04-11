{-# LANGUAGE ScopedTypeVariables #-}

-- | This module tests the Mealy automaton implementation.
module MealyAutomatonSpec (
    spec,
)
where

import qualified Data.List as List
import qualified Data.Map as Map
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
    mealyStates,
    mealyTransitions,
    mealyUpdateState,
    mealyWalk,
 )
import Test.Hspec (Spec, context, describe, it)
import Test.QuickCheck (Property, property, (==>))
import Utils (Input, Mealy (..), NonMinimalMealy (..), Output, State, findReachable, statesAreEquivalent)

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
