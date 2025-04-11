module WMethodSpec (
    spec,
) where

import BlackBox (accessSequences, globalCharacterizingSet, inputs)
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import Test.Hspec (Spec, context, describe, it)
import Test.QuickCheck (Property, property, (==>))
import Utils (ArbWMethod (..), Input, Mealy (..), Output, State)
import WMethod (WMethod (..), wmethod, wmethodSuite)
import qualified Data.Map as Map

prop_WMethodCardinality :: ArbWMethod -> Mealy Input Output State -> Bool
prop_WMethodCardinality (ArbWMethod (WMethod d)) (Mealy aut) =
    length (wmethodSuite (WMethod d) aut) == cardinality
  where
    alphabet = length (Set.toList $ inputs aut)
    accessSeqs = length (Map.elems $ accessSequences aut)
    characterizingSet = length (Set.toList $ globalCharacterizingSet aut)
    transitionCover = accessSeqs * alphabet
    cardinality = sum [transitionCover * (alphabet ^ n) * characterizingSet | n <- [0 .. d]]

prop_WMethodIdentity :: Mealy Input Output State -> Bool
prop_WMethodIdentity (Mealy aut) = isNothing (wmethod (WMethod 2) aut aut)

prop_WMethodDifference :: Mealy Input Output State -> Mealy Input Output State -> Property
prop_WMethodDifference (Mealy aut1) (Mealy aut2) = aut1 /= aut2 ==> isJust (wmethod (WMethod 3) aut1 aut2)

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

    describe "WMethod.wmethodSuite" $ do
        it "returns the correct cardinality" $
            property prop_WMethodCardinality
