{-# LANGUAGE ScopedTypeVariables #-}

-- | This module tests the Mealy automaton implementation.
module AutomatonSpec (
    spec,
)
where

import Control.Monad (replicateM)
import Control.Monad.Identity (runIdentity)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Haal.Automaton.MealyAutomaton (
    MealyAutomaton (..),
    mealyDelta,
    mealyLambda,
    mkMealyAutomaton,
 )
import Haal.BlackBox
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.QuickCheck (Property, forAll, property, (.&&.), (===), (==>))
import Utils (Input (..), Mealy (..), NonMinimalMealy (..), Output (..), genState, statesAreEquivalent)

-- The global characterizing set of a non minimal mealy automaton contains
-- the empty list. This will fail if 'stateSpace' has fewer than 6-7 states
-- because a lot of test cases will be discarded.
prop_emptyListInCharacterizingSet :: NonMinimalMealy -> Int -> Int -> Property
prop_emptyListInCharacterizingSet (NonMinimalMealy automaton) s1 s2 =
    statesAreEquivalent automaton s1 s2
        && s1
            /= s2
        ==> []
            `Set.member` globalCharacterizingSet automaton

-- Two states that are not equivalent can be distinguished.
prop_existsDistinguishingSequence :: Mealy Input Output -> Int -> Int -> Property
prop_existsDistinguishingSequence (Mealy automaton) s1 s2 =
    not (statesAreEquivalent automaton s1 s2) ==>
        output1
            /= output2
            && output1 /= []
            && output2 /= []
  where
    dist = distinguish automaton s1 s2
    (_, output1) = runIdentity $ walk (update automaton s1) dist
    (_, output2) = runIdentity $ walk (update automaton s2) dist

-- The map returned by 'mealyTransitions' is equivalent to the 'mealyLambda'
-- and 'mealyDelta' functions of the automaton.
prop_mappingEquivalentToFunctions :: Mealy Input Output -> Bool
prop_mappingEquivalentToFunctions (Mealy automaton) =
    let transs = transitions automaton
        alphabet = Set.toList $ inputs automaton
        sts = Set.toList $ states automaton
        -- Calculate outputs using mealyDelta and mealyLambda
        mapOutputs =
            [ Maybe.fromJust (Map.lookup (s, a) transs)
            | s <- sts
            , a <- alphabet
            ]
        funOutputs = [(mealyDelta automaton s a, mealyLambda automaton s a) | s <- sts, a <- alphabet]
     in mapOutputs == funOutputs

-- The access sequences returned by 'mealyAccessSequences' cover all reachable states.
prop_completeAccessSequences :: Mealy Input Output -> Property
prop_completeAccessSequences (Mealy automaton) = sts == rsts ==> allin
  where
    seqs = accessSequences automaton
    sts = states automaton
    rsts = reachable automaton
    allin = all (`Map.member` seqs) rsts

-- The access sequences returned by 'mealyAccessSequences' are the shortest
prop_shortestAccessSequences :: Mealy Input Output -> Int -> Int -> Property
prop_shortestAccessSequences (Mealy automaton) s1 s2 =
    s1 `Set.member` rsts
        && s2 `Set.member` rsts
        && existsS1toS2
        ==> List.length seq2 <= List.length seq1 + 1
  where
    rsts = reachable automaton
    transs = transitions automaton
    accessSeqs = accessSequences automaton
    seq1 = accessSeqs Map.! s1
    seq2 = accessSeqs Map.! s2
    -- find transition in map (s, i) -> (s, o)
    -- that leads from s1 to s2
    listed = Map.toList transs
    filtering (s, i) = s == s1 && fst (transs Map.! (s, i)) == s2
    maybeTransition = List.find filtering $ List.map fst listed
    existsS1toS2 = case maybeTransition of
        Nothing -> False
        Just _ -> True

{- | A counter modulo 5 that outputs 'Y' when input 'A' makes it wrap around and
'X' otherwise; every other input resets it.
-}
counter :: MealyAutomaton Int Input Output
counter = mkMealyAutomaton delta lambda (Set.fromList [0 .. 4]) 0
  where
    delta s A = (s + 1) `mod` 5
    delta _ _ = 0
    lambda 4 A = Y
    lambda _ _ = X

-- | 'counter' with its states renumbered, so a different but equivalent automaton.
renumberedCounter :: MealyAutomaton Int Input Output
renumberedCounter = mkMealyAutomaton delta lambda (Set.fromList [10 .. 14]) 10
  where
    delta s A = 10 + (s - 10 + 1) `mod` 5
    delta _ _ = 10
    lambda 14 A = Y
    lambda _ _ = X

-- | A single state that always outputs 'X'.
oneState :: MealyAutomaton Int Input Output
oneState = mkMealyAutomaton (\_ _ -> 0) (\_ _ -> X) (Set.fromList [0]) 0

-- | The outputs of an automaton on a word, from its initial state.
run :: MealyAutomaton Int Input Output -> [Input] -> [Output]
run aut = snd . walkPure (resetPure aut)

{- | The word 'difference' returns is a shortest witness: the two automata
differ on its last output, and agree on every word one symbol shorter. Outputs
are prefix-closed, so they then agree on every shorter word too. Witnesses of
more than 5 symbols are only checked for the first part, to keep the
enumeration small.
-}
prop_differenceIsShortestWitness :: Mealy Input Output -> Mealy Input Output -> Property
prop_differenceIsShortestWitness (Mealy a) (Mealy b) = case difference a b of
    Nothing -> property True
    Just w ->
        let (oa, ob) = (run a w, run b w)
            shorter = replicateM (length w - 1) [minBound .. maxBound]
            agreeOnShorter = length w > 5 || all (\v -> run a v == run b v) shorter
         in (last oa /= last ob) === True .&&. agreeOnShorter === True

-- | An automaton has no difference with itself.
prop_noDifferenceWithItself :: Mealy Input Output -> Property
prop_noDifferenceWithItself (Mealy a) = difference a a === Nothing

spec :: Spec
spec = do
    describe "BlackBox.difference" $ do
        it "finds the shortest word on which the counter and a one-state automaton differ" $ do
            let w = difference counter oneState
            w `shouldBe` Just [A, A, A, A, A]
            fmap (run counter) w `shouldBe` Just [X, X, X, X, Y]
        it "finds no difference between an automaton and itself" $
            difference counter counter `shouldBe` Nothing
        it "finds no difference between equivalent automata with different states" $
            difference counter renumberedCounter `shouldBe` Nothing
        it "returns a shortest witness" $
            property prop_differenceIsShortestWitness
        it "returns nothing for an automaton compared with itself" $
            property prop_noDifferenceWithItself

    describe "Blackbox.distinguish for MealyAutomaton" $
        context "if 2 automatons states are not equivalent" $
            it "returns an input sequence that distinguishes them" $
                property $ \aut ->
                    forAll genState $ \s1 ->
                        forAll genState $ \s2 ->
                            prop_existsDistinguishingSequence aut s1 s2

    describe "BlackBox.globalCharacterizingSet for MealyAutomaton" $
        context "if the automaton contains at least 2 equivalent states" $
            it "returns a set that contains the empty list" $
                property $ \aut ->
                    forAll genState $ \s1 ->
                        forAll genState $ \s2 ->
                            prop_emptyListInCharacterizingSet aut s1 s2

    describe "MealyAutomaton.mealyTransitions" $
        it "returns a map equivalent to the transition and output functions of the model" $
            property
                prop_mappingEquivalentToFunctions

    describe "BlackBox.accessSequences for MealyAutomaton" $ do
        it "returns a map from states to list of inputs that covers all reachable states" $
            property
                prop_completeAccessSequences

        it "returns a map from reachable states to shortest list of inputs that access them" $
            property $ \aut ->
                forAll genState $ \s1 ->
                    forAll genState $ \s2 ->
                        prop_shortestAccessSequences aut s1 s2
