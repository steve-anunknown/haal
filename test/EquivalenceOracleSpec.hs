module EquivalenceOracleSpec (
    spec,
) where

import Control.Monad.Reader
import qualified Data.Set as Set
import Haal.Automaton.MealyAutomaton (MealyAutomaton, mkMealyAutomaton)
import Haal.BlackBox (states)
import Haal.EquivalenceOracle.WMethod (
    RandomWMethodConfig (..),
    WMethodConfig (..),
    mkRandomWMethod,
    mkWMethod,
    wmethodSuiteSize,
 )
import Haal.EquivalenceOracle.WpMethod (
    RandomWpMethodConfig (..),
    WpMethodConfig (..),
    mkRandomWpMethod,
    mkWpMethod,
 )
import Haal.Experiment
import Haal.Learning.LMstar (LMstarConfig (..), mkLMstar)
import System.Random (mkStdGen)
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldNotBe, shouldSatisfy)
import Test.QuickCheck (Property, property, (==>))
import Utils

-- Generic identity and difference properties
prop_identity :: (OracleWrapper w oracle) => Mealy Input Output -> w -> Bool
prop_identity (Mealy aut) w = ([], []) == snd (runReader (findCex (unwrap w) aut) aut)

prop_difference :: (OracleWrapper w oracle) => Mealy Input Output -> Mealy Input Output -> w -> Property
prop_difference (Mealy aut1) (Mealy aut2) w =
    aut1 /= aut2 ==> ([], []) /= snd (runReader (findCex (unwrap w) aut1) aut2)

-- WMethod-specific cardinality law
prop_WMethodCardinality :: ArbWMethod -> Mealy Input Output -> Bool
prop_WMethodCardinality (ArbWMethod wm) (Mealy aut) =
    length (snd (testSuite wm aut)) == wmethodSuiteSize wm aut

{- | A counter modulo 5 that outputs 'Y' when input 'A' makes it wrap around and
'X' otherwise; every other input resets it. No single input tells its states
apart, so the first hypothesis of LM* has one state.
-}
counter :: MealyAutomaton Int Input Output
counter = mkMealyAutomaton delta lambda (Set.fromList [0 .. 4]) 0
  where
    delta s A = (s + 1) `mod` 5
    delta _ _ = 0
    lambda 4 A = Y
    lambda _ _ = X

-- | A hypothesis with a single state, which always outputs 'X'.
oneState :: MealyAutomaton Int Input Output
oneState = mkMealyAutomaton (\_ _ -> 0) (\_ _ -> X) (Set.fromList [0]) 0

-- | The test suite an oracle generates for the one-state hypothesis.
suiteForOneState :: (EquivalenceOracle oracle) => Either String oracle -> [[Input]]
suiteForOneState = either error (\o -> snd (testSuite o oneState))

-- | The counterexample an oracle finds for the one-state hypothesis of 'counter'.
cexForOneState :: (EquivalenceOracle oracle) => Either String oracle -> [Input]
cexForOneState = either error (\o -> fst (snd (runReader (findCex o oneState) counter)))

spec :: Spec
spec = do
    describe "A hypothesis with a single state" $ do
        -- Its characterizing set is empty. The oracles used to build no test
        -- words from it (W, Wp) or crash on it (random Wp).
        it "gets a non-empty W-method test suite" $
            suiteForOneState (mkWMethod (WMethodConfig 1)) `shouldNotBe` []
        it "gets a non-empty Wp-method test suite" $
            suiteForOneState (mkWpMethod (WpMethodConfig 1)) `shouldNotBe` []
        -- Summing the lengths generates every test word, which is where the
        -- random Wp-method crashed.
        it "gets a non-empty random W-method test suite" $
            sum (map length (suiteForOneState (mkRandomWMethod (RandomWMethodConfig (mkStdGen 1) 20 6))))
                `shouldSatisfy` (> 0)
        it "gets a non-empty random Wp-method test suite" $
            sum (map length (suiteForOneState (mkRandomWpMethod (RandomWpMethodConfig (mkStdGen 1) 4 3 20))))
                `shouldSatisfy` (> 0)
        it "is refuted by the W-method with enough extra states" $
            cexForOneState (mkWMethod (WMethodConfig 4)) `shouldNotBe` []
        it "is refuted by the Wp-method with enough extra states" $
            cexForOneState (mkWpMethod (WpMethodConfig 4)) `shouldNotBe` []
        it "does not stop LM* from learning all states of the counter" $ do
            let oracle = either error id (mkWMethod (WMethodConfig 4))
                model = fst (runExperiment (experiment (mkLMstar Star) oracle) counter)
            Set.size (states model) `shouldBe` 5

    describe "WMethod Equivalence Oracle" $ do
        context "when two automatons differ" $
            it "WMethod returns Just" $
                property (prop_difference :: Mealy Input Output -> Mealy Input Output -> ArbWMethod -> Property)

        context "when two automatons are the same" $
            it "WMethod returns Nothing" $
                property (prop_identity :: Mealy Input Output -> ArbWMethod -> Bool)

        it "computes the correct WMethod test suite size" $
            property prop_WMethodCardinality

    describe "WpMethod Equivalence Oracle" $ do
        context "when two automatons differ" $
            it "WpMethod returns Just" $
                property (prop_difference :: Mealy Input Output -> Mealy Input Output -> ArbWpMethod -> Property)

        context "when two automatons are the same" $
            it "WpMethod returns Nothing" $
                property (prop_identity :: Mealy Input Output -> ArbWpMethod -> Bool)

    describe "RandomWords Equivalence Oracle" $ do
        context "when two automatons differ" $
            it "RandomWords returns Just" $
                property (prop_difference :: Mealy Input Output -> Mealy Input Output -> ArbRandomWords -> Property)

        context "when two automatons are the same" $
            it "RandomWords returns Nothing" $
                property (prop_identity :: Mealy Input Output -> ArbRandomWords -> Bool)

    describe "RandomWalk Equivalence Oracle" $ do
        context "when two automatons differ" $
            it "RandomWalk returns Just" $
                property (prop_difference :: Mealy Input Output -> Mealy Input Output -> ArbRandomWalk -> Property)

        context "when two automatons are the same" $
            it "RandomWalk returns Nothing" $
                property (prop_identity :: Mealy Input Output -> ArbRandomWalk -> Bool)

    describe "RandomWMethod Equivalence Oracle" $ do
        context "when two automatons differ" $
            it "RandomWMethod returns Just" $
                property (prop_difference :: Mealy Input Output -> Mealy Input Output -> ArbRandomWMethod -> Property)

        context "when two automatons are the same" $
            it "RandomWMethod returns Nothing" $
                property (prop_identity :: Mealy Input Output -> ArbRandomWMethod -> Bool)

    describe "RandomWpMethod Equivalence Oracle" $ do
        context "when two automatons differ" $
            it "RandomWpMethod returns Just" $
                property (prop_difference :: Mealy Input Output -> Mealy Input Output -> ArbRandomWpMethod -> Property)

        context "when two automatons are the same" $
            it "RandomWpMethod returns Nothing" $
                property (prop_identity :: Mealy Input Output -> ArbRandomWpMethod -> Bool)
