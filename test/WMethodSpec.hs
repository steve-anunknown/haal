module WMethodSpec (
    spec,
) where

import Control.Monad.Reader
import Data.Maybe (isJust, isNothing)
import EquivalenceOracle.WMethod (WMethod (..), wmethodSuiteSize)
import Experiment
import Test.Hspec (Spec, context, describe, it)
import Test.QuickCheck (Property, property, (==>))
import Utils (ArbWMethod (..), Input, Mealy (..), Output, State)

prop_WMethodCardinality :: ArbWMethod -> Mealy State Input Output -> Bool
prop_WMethodCardinality (ArbWMethod (WMethod d)) (Mealy aut) =
    (length (snd (testSuite (WMethod d) aut))) == wmethodSuiteSize (WMethod d) aut

prop_WMethodIdentity :: Mealy State Input Output -> Bool
prop_WMethodIdentity (Mealy aut) = isNothing $ snd (runReader (findCex (WMethod 2) aut) aut)

prop_WMethodDifference :: Mealy State Input Output -> Mealy State Input Output -> Property
prop_WMethodDifference (Mealy aut1) (Mealy aut2) = aut1 /= aut2 ==> isJust $ snd (runReader (findCex (WMethod 3) aut1) aut2)

spec :: Spec
spec = do
    describe "WMethod.wmethod" $ do
        context "if 2 automatons are not the same" $
            it "returns Just" $
                property
                    prop_WMethodDifference

        context "if 2 automatons are the same" $
            it "returns Nothing" $
                property prop_WMethodIdentity

    describe "WMethod.wmethodSuiteSize" $ do
        it "returns the correct cardinality" $
            property prop_WMethodCardinality
