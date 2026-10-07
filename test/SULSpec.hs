{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Regression tests for the reset contract between the library and a SUL.

Active automata learning assumes that queries are independent, i.e. that the
SUL is reset before every membership query and every equivalence test case.
These tests check that the learned model does not depend on the state the SUL
is left in by earlier queries, or on the state it is handed over in.
-}
module SULSpec (
    spec,
) where

import Control.Exception (ErrorCall (..), evaluate)
import Control.Monad.Identity (Identity)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.List as List
import qualified Data.Set as Set
import Haal.Automaton.MealyAutomaton (MealyAutomaton, mkMealyAutomaton)
import Haal.BlackBox
import Haal.EquivalenceOracle.WpMethod (WpMethod, WpMethodConfig (..), mkWpMethod)
import Haal.Experiment (experiment, runExperiment, runExperimentT)
import Haal.Learning.LMstar (LMstarConfig (..), mkLMstar)
import Test.Hspec (Spec, describe, it, shouldThrow)
import Test.QuickCheck (Property, ioProperty, property, within, (===))
import Utils (Input (..), Mealy (..), Output (..))

{- | A SUL that wraps a stateful system. The automaton lives behind an 'IORef',
so 'step' and 'reset' mutate it and hand back the same handle, just like a
driver for a real process or socket would.
-}
newtype RefSUL i o = RefSUL (IORef (MealyAutomaton Int i o))

instance SUL RefSUL IO where
    step h@(RefSUL ref) i = do
        aut <- readIORef ref
        let (aut', o) = stepPure aut i
        writeIORef ref aut'
        return (h, o)
    reset h@(RefSUL ref) = do
        modifyIORef' ref resetPure
        return h

type Model = MealyAutomaton Int Input Output

{- | Without resets the answers to queries depend on earlier queries, so the
oracle can keep finding counterexamples that refinement never fixes, and the
experiment never terminates. Bound every property so that this shows up as a
failure instead of a hang.
-}
terminates :: Property -> Property
terminates = within 5000000

oracle :: WpMethod
oracle = either error id (mkWpMethod (WpMethodConfig 1))

-- | Learn an automaton purely, using the automaton itself as the SUL.
learnPure :: LMstarConfig -> Model -> Model
learnPure cfg aut = fst (runExperiment (experiment (mkLMstar cfg) oracle) aut)

-- | Learn an automaton through a 'RefSUL' wrapping it.
learnRef :: LMstarConfig -> Model -> IO Model
learnRef cfg aut = do
    ref <- newIORef aut
    fst <$> runExperimentT (experiment (mkLMstar cfg) oracle) (RefSUL ref)

{- | A stateful SUL must learn the same model as its pure counterpart. Both
runs ask the same queries, so the answers, and the models, can only differ if
some query is not preceded by a reset.
-}
prop_statefulMatchesPure :: LMstarConfig -> Mealy Input Output -> Property
prop_statefulMatchesPure cfg (Mealy aut) = terminates $ ioProperty $ do
    let aut0 = resetPure aut
    learned <- learnRef cfg aut0
    return (learned === learnPure cfg aut0)

{- | The learned model must not depend on the current state of the SUL it is
given; learning has to start from the initial state. The 'Arbitrary' instance
of 'Mealy' picks a random current state.
-}
prop_currentStateIrrelevant :: LMstarConfig -> Mealy Input Output -> Property
prop_currentStateIrrelevant cfg (Mealy aut) =
    terminates $
        learnPure cfg aut === learnPure cfg (resetPure aut)

{- | A pure SUL that overrides 'query'. With @Correct@ the override computes the
same outputs as the default; with @DropsOutput@ it loses the last output,
breaking the contract of 'query'.
-}
data Override = Correct | DropsOutput

data OverridingSUL i o = OverridingSUL Override (MealyAutomaton Int i o)

instance SUL OverridingSUL Identity where
    step (OverridingSUL ov aut) i =
        let (aut', o) = stepPure aut i
         in return (OverridingSUL ov aut', o)
    reset (OverridingSUL ov aut) = return (OverridingSUL ov (resetPure aut))
    query (OverridingSUL ov aut) xs =
        let os = snd (walkPure (resetPure aut) xs)
         in return $ case ov of
                Correct -> os
                DropsOutput -> List.take (length os - 1) os

-- | A small fixed automaton: it counts @A@s modulo 3 and outputs the count.
counter :: Model
counter = mkMealyAutomaton delta lambda (Set.fromList [0, 1, 2]) 0
  where
    delta s A = (s + 1) `mod` 3
    delta s _ = s
    lambda s A = [X, Y, Z] !! ((s + 1) `mod` 3)
    lambda _ _ = W

learnOverriding :: Override -> Model -> Model
learnOverriding ov aut =
    fst (runExperiment (experiment (mkLMstar Star) oracle) (OverridingSUL ov aut))

isContractViolation :: ErrorCall -> Bool
isContractViolation (ErrorCall msg) = "one output per input" `List.isInfixOf` msg

spec :: Spec
spec = do
    describe "Overriding query" $ do
        it "a correct override learns the same model as the default" $
            learnOverriding Correct counter == learnPure Star counter
        it "an override returning too few outputs fails with a clear error" $
            evaluate (length (show (learnOverriding DropsOutput counter)))
                `shouldThrow` isContractViolation

    describe "Learning through a stateful SUL" $ do
        it "LM* learns the same model as through a pure SUL" $
            property (prop_statefulMatchesPure Star)
        it "LM+ learns the same model as through a pure SUL" $
            property (prop_statefulMatchesPure Plus)

    describe "Learning from a SUL in a non-initial state" $ do
        it "LM* learns the same model as from the initial state" $
            property (prop_currentStateIrrelevant Star)
        it "LM+ learns the same model as from the initial state" $
            property (prop_currentStateIrrelevant Plus)
