{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Tests for the statistics wrappers and the experiment hooks.
module StatisticsSpec (
    spec,
) where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State (State, modify', runState, runStateT)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Haal.Automaton.MealyAutomaton (MealyAutomaton)
import Haal.BlackBox
import Haal.EquivalenceOracle.WpMethod (WpMethod, WpMethodConfig (..), mkWpMethod)
import Haal.Experiment (experiment, experimentWith, runExperiment, runExperimentT)
import Haal.Learning.LMstar (LMstarConfig (..), mkLMstar)
import Haal.Statistics
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Property, ioProperty, property, (.&&.), (===))
import Utils (Input, Mealy (..), Output)

type Model = MealyAutomaton Int Input Output

oracle :: WpMethod
oracle = either error id (mkWpMethod (WpMethodConfig 1))

-- | Learn an automaton purely, without any statistics.
learnPlain :: LMstarConfig -> Model -> Model
learnPlain cfg aut = runExperiment (experiment (mkLMstar cfg) oracle) aut

{- | A SUL that counts its own resets and steps in an 'IORef', independently of
'Counted'. It does not override 'query', so every query goes through its 'reset'
and 'step'.
-}
data Spy i o = Spy (IORef (Int, Int)) (MealyAutomaton Int i o)

instance SUL Spy IO where
    step (Spy ref aut) i = do
        modifyIORef' ref (\(q, s) -> (q, s + 1))
        let (aut', o) = stepPure aut i
        return (Spy ref aut', o)
    reset (Spy ref aut) = do
        modifyIORef' ref (\(q, s) -> (q + 1, s))
        return (Spy ref (resetPure aut))

-- | 'experimentWith' 'noHooks' is the same experiment as 'experiment'.
prop_noHooksIsExperiment :: LMstarConfig -> Mealy Input Output -> Property
prop_noHooksIsExperiment cfg (Mealy aut) =
    runExperiment (experimentWith noHooks (mkLMstar cfg) oracle) aut === learnPlain cfg aut

-- | Counting the queries does not change what is learned.
prop_countedLearnsSameModel :: LMstarConfig -> Mealy Input Output -> Property
prop_countedLearnsSameModel cfg (Mealy aut) =
    let counted = runExperimentT (experiment (mkLMstar cfg) oracle) (Counted aut)
        (model, _) = runIdentity (runStateT counted initialCounts)
     in model === learnPlain cfg aut

{- | 'Counted' agrees with the SUL's own count. 'Counted' overrides 'query', so
it never sees the inner SUL's 'reset' and 'step', yet it must arrive at the same
numbers: one query per reset and one symbol per step.
-}
prop_countedMatchesOwnCount :: LMstarConfig -> Mealy Input Output -> Property
prop_countedMatchesOwnCount cfg (Mealy aut) = ioProperty $ do
    ref <- newIORef (0, 0)
    let counted = runExperimentT (experiment (mkLMstar cfg) oracle) (Counted (Spy ref aut))
    (model, counts) <- runStateT counted initialCounts
    (resets, steps) <- readIORef ref
    return $
        (queries counts, symbols counts) === (resets, steps)
            .&&. model === learnPlain cfg aut

{- | The hooks are called at the documented moments: every hypothesis and every
counterexample is reported, and the phases alternate, starting with 'Learning'
and ending with 'Testing' on the final hypothesis. Each counterexample ends a
round, so there is exactly one more hypothesis than counterexamples.
-}
prop_hooksReportEvents :: LMstarConfig -> Mealy Input Output -> Property
prop_hooksReportEvents cfg (Mealy aut) = ioProperty $ do
    ref <- newIORef (0, 0)
    phases <- newIORef []
    hyps <- newIORef (0 :: Int)
    cexs <- newIORef []
    let hooks =
            Hook
                { onPhase = \p -> modifyIORef' phases (p :)
                , onHypothesis = \_ -> modifyIORef' hyps (+ 1)
                , onCounterexample = \c -> modifyIORef' cexs (c :)
                }
    model <- runExperimentT (experimentWith hooks (mkLMstar cfg) oracle) (Spy ref aut)
    ps <- reverse <$> readIORef phases
    nHyps <- readIORef hyps
    cs <- readIORef cexs
    return $
        model === learnPlain cfg aut
            .&&. nHyps === length cs + 1
            .&&. all (not . null) cs === True
            .&&. ps === take (2 * nHyps) (cycle [Learning, Testing])

-- | A statistic built only from hooks: the number of hypotheses and the counterexamples.
data Rounds = Rounds {hypotheses :: !Int, counterexamples :: [[Input]]}
    deriving (Show, Eq)

roundsHooks :: Hook (State Rounds) MealyAutomaton Input Output
roundsHooks =
    noHooks
        { onHypothesis = \_ -> modify' (\r -> r{hypotheses = hypotheses r + 1})
        , onCounterexample = \c -> modify' (\r -> r{counterexamples = c : counterexamples r})
        }

{- | A pure automaton can be learned directly inside a user's state monad, with
statistics gathered only through hooks: no SUL wrapper is needed, because an
automaton is a SUL in any monad.
-}
prop_hooksOnPlainAutomaton :: LMstarConfig -> Mealy Input Output -> Property
prop_hooksOnPlainAutomaton cfg (Mealy aut) =
    let run = runExperimentT (experimentWith roundsHooks (mkLMstar cfg) oracle) aut
        (model, rounds) = runState run (Rounds 0 [])
     in model === learnPlain cfg aut
            .&&. hypotheses rounds === length (counterexamples rounds) + 1

spec :: Spec
spec = do
    describe "experimentWith" $ do
        it "with noHooks, LM* learns the same model as experiment" $
            property (prop_noHooksIsExperiment Star)
        it "with noHooks, LM+ learns the same model as experiment" $
            property (prop_noHooksIsExperiment Plus)
        it "reports every hypothesis, counterexample and phase change (LM*)" $
            property (prop_hooksReportEvents Star)
        it "reports every hypothesis, counterexample and phase change (LM+)" $
            property (prop_hooksReportEvents Plus)
        it "gathers hook-only statistics on a plain automaton in State (LM*)" $
            property (prop_hooksOnPlainAutomaton Star)
        it "gathers hook-only statistics on a plain automaton in State (LM+)" $
            property (prop_hooksOnPlainAutomaton Plus)

    describe "Counted" $ do
        it "does not change the model LM* learns" $
            property (prop_countedLearnsSameModel Star)
        it "does not change the model LM+ learns" $
            property (prop_countedLearnsSameModel Plus)
        it "agrees with the SUL's own count of resets and steps (LM*)" $
            property (prop_countedMatchesOwnCount Star)
        it "agrees with the SUL's own count of resets and steps (LM+)" $
            property (prop_countedMatchesOwnCount Plus)
