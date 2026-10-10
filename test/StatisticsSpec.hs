{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Tests for the statistics of an experiment.
module StatisticsSpec (
    spec,
) where

import qualified Control.Foldl as L
import Control.Monad (forM_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import Haal.Automaton.MealyAutomaton (MealyAutomaton, mkMealyAutomaton)
import Haal.BlackBox
import Haal.EquivalenceOracle.WpMethod (WpMethod, WpMethodConfig (..), mkWpMethod)
import Haal.Experiment (experimentWith, measuredExperiment, runExperiment, runExperimentT)
import Haal.Learning.LMstar (LMstarConfig (..), mkLMstar)
import Haal.Statistics
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

data I = Tick | Clear deriving (Show, Eq, Ord, Enum, Bounded)
data O = Quiet | Wrap deriving (Show, Eq, Ord, Enum, Bounded)

{- | A counter modulo 5 that outputs 'Wrap' only when it wraps around. LM*'s
first hypothesis cannot tell the states apart, so learning it takes a few
counterexamples, which exercises both phases.
-}
counter :: MealyAutomaton Int I O
counter = mkMealyAutomaton delta lambda (Set.fromList [0 .. 4]) 0
  where
    delta s Tick = (s + 1) `mod` 5
    delta _ Clear = 0
    lambda 4 Tick = Wrap
    lambda _ _ = Quiet

-- | Depth 4: enough to refute LM*'s one-state first hypothesis of the 5-state 'counter'.
oracle :: WpMethod
oracle = either error id (mkWpMethod (WpMethodConfig 4))

{- | A SUL that counts its own resets and steps, per phase, in 'IORef's,
independently of the statistics. Its phase is set from outside. It does not
override 'query', so every query goes through its 'reset' and 'step'.
-}
data Spy i o = Spy
    { spyPhase :: IORef Phase
    , spyCounts :: IORef (Tally, Tally)
    -- ^ resets and steps in the 'Learning' and in the 'Testing' phase
    , spyAut :: MealyAutomaton Int i o
    }

spyTick :: Spy i o -> Int -> Int -> IO ()
spyTick spy r s = do
    p <- readIORef (spyPhase spy)
    let add (Tally r0 s0) = Tally (r0 + r) (s0 + s)
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

spec :: Spec
spec = forM_ [("LM*", Star), ("LM+", Plus)] $ \(name, cfg) -> describe name $ do
    let learner = mkLMstar cfg

    it "counts the same queries and symbols per phase as the SUL counts itself" $ do
        spy <- Spy <$> newIORef Learning <*> newIORef (Tally 0 0, Tally 0 0) <*> pure counter
        events <- newIORef []
        let emit e = do
                case e of
                    PhaseChanged p -> writeIORef (spyPhase spy) p
                    _ -> return ()
                modifyIORef' events (e :)
        _ <- runExperimentT (experimentWith emit learner oracle) spy
        own <- readIORef (spyCounts spy)
        stats <- L.fold statistics . reverse <$> readIORef events
        (learning stats, testing stats) `shouldBe` own
        queries (testing stats) `shouldSatisfy` (> 0)

    it "measuredExperiment gives the same statistics as folding the emitted events" $ do
        events <- newIORef []
        _ <- runExperimentT (experimentWith (\e -> modifyIORef' events (e :)) learner oracle) counter
        folded <- L.fold statistics . reverse <$> readIORef events
        snd (runExperiment (measuredExperiment statistics learner oracle) counter) `shouldBe` folded

    it "combined with another fold, gives the same statistics as on its own" $ do
        let measure stat = runExperiment (measuredExperiment stat learner oracle) counter
            (model, (stats, nEvents)) = measure ((,) <$> statistics <*> L.length)
            (_, alone) = measure statistics
        stats `shouldBe` alone
        nEvents `shouldSatisfy` (> 0)
        rounds stats `shouldSatisfy` (> 0)
        take 1 (hypotheses stats) `shouldBe` [model]
