{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Tests for the statistics wrappers and the experiment hooks.
module StatisticsSpec (
    spec,
) where

import Control.Monad.State (runStateT)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Haal.Automaton.MealyAutomaton (MealyAutomaton)
import Haal.BlackBox
import Haal.EquivalenceOracle.WpMethod (WpMethod, WpMethodConfig (..), mkWpMethod)
import Haal.Experiment (experiment, experimentWith, runExperiment, runExperimentT)
import Haal.Learning.LMstar (LMstarConfig (..), mkLMstar)
import Haal.Statistics
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.QuickCheck (Property, ioProperty, property, (.&&.), (===))
import Utils (Input, Mealy (..), Output)

type Model = MealyAutomaton Int Input Output

oracle :: WpMethod
oracle = either error id (mkWpMethod (WpMethodConfig 1))

-- | Learn an automaton purely, without any statistics.
learnPlain :: LMstarConfig -> Model -> Model
learnPlain cfg = runExperiment (experiment (mkLMstar cfg) oracle)

{- | A SUL that counts its own resets and steps, per phase, in 'IORef's,
independently of 'Counted'. Its phase is set from outside, by 'spyHooks'. It
does not override 'query', so every query goes through its 'reset' and 'step'.
-}
data Spy i o = Spy
    { spyPhase :: IORef Phase
    , spyCounts :: IORef ((Int, Int), (Int, Int))
    -- ^ (resets, steps) in the 'Learning' and in the 'Testing' phase
    , spyAut :: MealyAutomaton Int i o
    }

newSpy :: MealyAutomaton Int i o -> IO (Spy i o)
newSpy aut = Spy <$> newIORef Learning <*> newIORef ((0, 0), (0, 0)) <*> pure aut

-- | Add resets and steps to the counts of the spy's current phase.
spyTick :: Spy i o -> Int -> Int -> IO ()
spyTick spy r s = do
    p <- readIORef (spyPhase spy)
    let add (r0, s0) = (r0 + r, s0 + s)
    modifyIORef' (spyCounts spy) $ \(l, t) -> case p of
        Learning -> (add l, t)
        Testing -> (l, add t)

instance SUL Spy IO where
    step spy i = do
        spyTick spy 0 1
        let (aut', o) = stepPure (spyAut spy) i
        return (spy{spyAut = aut'}, o)
    reset spy = do
        spyTick spy 1 0
        return (spy{spyAut = resetPure (spyAut spy)})

-- | Hooks that keep the spy's phase up to date.
spyHooks :: Spy i o -> Hook IO aut i o
spyHooks spy = noHooks{onPhase = writeIORef (spyPhase spy)}

asTally :: (Int, Int) -> Tally
asTally (r, s) = Tally r s

{- | 'Counted' agrees with the SUL's own count. 'Counted' overrides 'query', so
it never sees the inner SUL's 'reset' and 'step', yet it must arrive at the same
numbers: one query per reset and one symbol per step. Without 'countingHooks',
everything counts as 'Learning'.
-}
prop_countedMatchesOwnCount :: LMstarConfig -> Mealy Input Output -> Property
prop_countedMatchesOwnCount cfg (Mealy aut) = ioProperty $ do
    spy <- newSpy aut
    let counted = runExperimentT (experiment (mkLMstar cfg) oracle) (Counted spy)
    (model, counts) <- runStateT counted initialCounts
    (own, _) <- readIORef (spyCounts spy)
    return $
        total counts
            === asTally own
            .&&. testing counts
                === Tally 0 0
            .&&. model
                === learnPlain cfg aut

{- | With 'countingHooks', 'Counted' splits the counts into the membership
queries sent while constructing hypotheses ('Learning') and those sent while
validating them ('Testing'), in agreement with the spy,
whose phase is set by its own hooks. This also exercises combining hooks with
'<>' and running hooks from an inner monad with 'liftHook'.
-}
prop_countingHooksSplitPhases :: LMstarConfig -> Mealy Input Output -> Property
prop_countingHooksSplitPhases cfg (Mealy aut) = ioProperty $ do
    spy <- newSpy aut
    let hooks = countingHooks <> liftHook (spyHooks spy)
        counted = runExperimentT (experimentWith hooks (mkLMstar cfg) oracle) (Counted spy)
    (model, counts) <- runStateT counted initialCounts
    (ownLearning, ownTesting) <- readIORef (spyCounts spy)
    return $
        learning counts
            === asTally ownLearning
            .&&. testing counts
                === asTally ownTesting
            .&&. (queries (testing counts) > 0)
                === True
            .&&. model
                === learnPlain cfg aut

-- | Hooks that append a label to a log, for checking how hooks combine.
logHook :: IORef [String] -> String -> Hook IO MealyAutomaton Input Output
logHook ref label = noHooks{onPhase = \_ -> modifyIORef' ref (++ [label])}

spec :: Spec
spec = do
    describe "Hook" $ do
        it "combined hooks run both, left first" $ do
            ref <- newIORef []
            onPhase (logHook ref "a" <> logHook ref "b") Learning
            readIORef ref `shouldReturn` ["a", "b"]

    describe "Counted" $ do
        it "agrees with the SUL's own count of resets and steps (LM*)" $
            property (prop_countedMatchesOwnCount Star)
        it "agrees with the SUL's own count of resets and steps (LM+)" $
            property (prop_countedMatchesOwnCount Plus)
        it "splits queries into construction and validation with countingHooks (LM*)" $
            property (prop_countingHooksSplitPhases Star)
        it "splits queries into construction and validation with countingHooks (LM+)" $
            property (prop_countingHooksSplitPhases Plus)
