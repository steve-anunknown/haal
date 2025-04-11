module WMethodSpec (
    spec,
) where

import Data.Maybe (isJust, isNothing)
import Test.Hspec (Spec, context, describe, it)
import Test.QuickCheck (Property, property, quickCheck, (==>))
import Test.QuickCheck.Property (withMaxSuccess)
import Utils (Input, Mealy (..), Output, State)
import WMethod (WMethod (..), wmethod)

prop_WMethodIdentity :: Mealy Input Output State -> Bool
prop_WMethodIdentity (Mealy aut) = isNothing (wmethod (WMethod 2) aut aut)

prop_WMethodDifference :: Mealy Input Output State -> Mealy Input Output State -> Property
prop_WMethodDifference (Mealy aut1) (Mealy aut2) =
    aut1 /= aut2 ==> isJust (wmethod (WMethod 4) aut1 aut2)

spec :: Spec
spec = do
    describe "WMethod.wmethod" $ do
        context "if 2 automatons are not the same" $
            it "returns Just" $
                property
                    prop_WMethodDifference

        context "if 2 automatons are the same" $
            it "returns Nothing" $
                quickCheck (withMaxSuccess 10 prop_WMethodIdentity)
