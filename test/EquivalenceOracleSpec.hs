module EquivalenceOracleSpec (
    spec,
) where

import Control.Monad.Reader
import EquivalenceOracle.WMethod (WMethod (..), wmethodSuiteSize)
import Experiment
import Test.Hspec (Spec, context, describe, it)
import Test.QuickCheck (Property, property, (==>))
import Utils

-- Generic identity and difference properties
prop_identity :: (OracleWrapper w oracle) => Mealy State Input Output -> w -> Bool
prop_identity (Mealy aut) w = ([], []) == (snd $ runReader (findCex (unwrap w) aut) aut)

prop_difference :: (OracleWrapper w oracle) => Mealy State Input Output -> Mealy State Input Output -> w -> Property
prop_difference (Mealy aut1) (Mealy aut2) w =
    aut1 /= aut2 ==> ([], []) /= (snd $ runReader (findCex (unwrap w) aut1) aut2)

-- WMethod-specific cardinality law
prop_WMethodCardinality :: ArbWMethod -> Mealy State Input Output -> Bool
prop_WMethodCardinality (ArbWMethod (WMethod d)) (Mealy aut) =
    length (snd (testSuite (WMethod d) aut)) == wmethodSuiteSize (WMethod d) aut

spec :: Spec
spec = do
    describe "WMethod Equivalence Oracle" $ do
        context "when two automatons differ" $
            it "WMethod returns Just" $
                property (prop_difference :: Mealy State Input Output -> Mealy State Input Output -> ArbWMethod -> Property)

        context "when two automatons are the same" $
            it "WMethod returns Nothing" $
                property (prop_identity :: Mealy State Input Output -> ArbWMethod -> Bool)

        it "computes the correct WMethod test suite size" $
            property prop_WMethodCardinality

    describe "WpMethod Equivalence Oracle" $ do
        context "when two automatons differ" $
            it "WpMethod returns Just" $
                property (prop_difference :: Mealy State Input Output -> Mealy State Input Output -> ArbWpMethod -> Property)

        context "when two automatons are the same" $
            it "WpMethod returns Nothing" $
                property (prop_identity :: Mealy State Input Output -> ArbWpMethod -> Bool)

    describe "RandomWords Equivalence Oracle" $ do
        context "when two automatons differ" $
            it "RandomWords returns Just" $
                property (prop_difference :: Mealy State Input Output -> Mealy State Input Output -> ArbRandomWords -> Property)

        context "when two automatons are the same" $
            it "RandomWords returns Nothing" $
                property (prop_identity :: Mealy State Input Output -> ArbRandomWords -> Bool)

    describe "RandomWalk Equivalence Oracle" $ do
        context "when two automatons differ" $
            it "RandomWalk returns Just" $
                property (prop_difference :: Mealy State Input Output -> Mealy State Input Output -> ArbRandomWalk -> Property)

        context "when two automatons are the same" $
            it "RandomWalk returns Nothing" $
                property (prop_identity :: Mealy State Input Output -> ArbRandomWalk -> Bool)

    describe "RandomWMethod Equivalence Oracle" $ do
        context "when two automatons differ" $
            it "RandomWMethod returns Just" $
                property (prop_difference :: Mealy State Input Output -> Mealy State Input Output -> ArbRandomWMethod -> Property)

        context "when two automatons are the same" $
            it "RandomWMethod returns Nothing" $
                property (prop_identity :: Mealy State Input Output -> ArbRandomWMethod -> Bool)

    describe "RandomWpMethod Equivalence Oracle" $ do
        context "when two automatons differ" $
            it "RandomWpMethod returns Just" $
                property (prop_difference :: Mealy State Input Output -> Mealy State Input Output -> ArbRandomWpMethod -> Property)

        context "when two automatons are the same" $
            it "RandomWpMethod returns Nothing" $
                property (prop_identity :: Mealy State Input Output -> ArbRandomWpMethod -> Bool)
