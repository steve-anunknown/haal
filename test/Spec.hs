{-# LANGUAGE DeriveDataTypeable #-}

module Main (
    main,
)
where

import qualified Data.Data as Data
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import MealyAutomaton
import Test.Hspec (describe, hspec, it, shouldBe)

data Input = A | B | C deriving (Show, Eq, Ord, Data.Data)
data Output = X | Y | Z deriving (Show, Eq, Ord, Data.Data)
data State = S0 | S1 | S2 deriving (Show, Eq, Ord, Data.Data)

triangleDelta :: State -> Input -> State
triangleDelta S0 _ = S1
triangleDelta S1 _ = S2
triangleDelta S2 _ = S0

triangleLambda :: State -> Input -> Output
triangleLambda _ A = X
triangleLambda _ B = Y
triangleLambda _ C = Z

triangleModel :: MealyAutomaton Input Output State
triangleModel = mkMealyAutomaton triangleDelta triangleLambda S0

main :: IO ()
main = hspec $ do
    describe "MealyAutomaton.mealyDistinguishingSequence" $ do
        it "returns the empty list if the automaton states cannot be distinguished" $ do
            mealyDistinguishingSequence triangleModel S0 S1 `shouldBe` []
            mealyDistinguishingSequence triangleModel S1 S2 `shouldBe` []
            mealyDistinguishingSequence triangleModel S2 S0 `shouldBe` []

    describe "MealyAutomaton.mealyLocalCharacterizingSet" $ do
        it "returns the singleton set with the empty list if all automaton states cannot be distinguished" $ do
            mealyLocalCharacterizingSet triangleModel S0 `shouldBe` Set.singleton []
            mealyLocalCharacterizingSet triangleModel S1 `shouldBe` Set.singleton []
            mealyLocalCharacterizingSet triangleModel S2 `shouldBe` Set.singleton []

    describe "MealyAutomaton.mealyGlobalCharacterizingSet" $ do
        it "returns the singleton set with the empty list if all automaton states cannot be distinguished" $ do
            mealyGlobalCharacterizingSet triangleModel `shouldBe` Set.singleton []

    describe "MealyAutomaton.mealyTransitions" $ do
        it "returns a map equivalent to the transition and output functions of the model" $ do
            let transitions = mealyTransitions triangleModel
                alphabet = Set.toList $ mealyAlphabet triangleModel
                states = Set.toList $ mealyStates triangleModel
                mapOutputs = [Maybe.fromJust (Map.lookup (s, a) transitions) |
                                s <- states, a <- alphabet]
                funOutputs = [(triangleDelta s a, triangleLambda s a) | s <- states, a <- alphabet]
            mapOutputs `shouldBe` funOutputs
