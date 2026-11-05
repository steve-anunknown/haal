{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Tests for 'Haal.SUL.Cached': a caching wrapper that answers repeated
membership queries (or a prefix of one) from a 'Haal.Trie.Trie' instead of the
underlying SUL, with the cache threaded through a 'StateT' layer rather than
any mutable state of its own.
-}
module CachedSpec (
    spec,
) where

import Control.Monad.State (State, modify', runState, runStateT)
import qualified Data.Set as Set
import Haal.Automaton.MealyAutomaton (MealyAutomaton, mkMealyAutomaton)
import Haal.BlackBox (SUL (..), resetPure, stepPure, walkPure)
import Haal.EquivalenceOracle.WpMethod (WpMethodConfig (..), mkWpMethod)
import Haal.Experiment (cachedExperiment, experiment, runExperiment)
import Haal.Learning.LMstar (LMstarConfig (Star), mkLMstar)
import Haal.SUL.Cached (CacheState (..), CachedSUL (..), emptyCacheState)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (Property, property, (.&&.), (===))
import Utils (Input (..), Mealy (..), Output (..))

{- | A SUL that counts every query actually sent to it, in the base 'State'
monad, so tests can tell whether 'Haal.SUL.Cached.CachedSUL' really skipped
the underlying system or just happened to compute the same answer.
-}
newtype CountingSUL i o = CountingSUL (MealyAutomaton Int i o)

instance (Ord i) => SUL CountingSUL i o (State Int) where
    step (CountingSUL aut) i = do
        let (aut', o) = stepPure aut i
        pure (CountingSUL aut', o)
    reset (CountingSUL aut) = pure (CountingSUL (resetPure aut))
    query (CountingSUL aut) is = do
        modify' (+ 1)
        pure (snd (walkPure (resetPure aut) is))

-- | A 2-state automaton over 'Input': @A@ toggles the state, every other input is quiet.
toggle :: MealyAutomaton Int Input Output
toggle = mkMealyAutomaton delta lambda (Set.fromList [0, 1]) 0
  where
    delta 0 A = 1
    delta 1 A = 0
    delta s _ = s
    lambda 0 A = X
    lambda 1 A = Y
    lambda _ _ = Z

{- | Run a sequence of queries against a SUL through a 'CachedSUL', threading
one 'CacheState' through all of them, and return the outputs of each query,
the final cache, and how many queries actually reached the underlying SUL.
-}
runCached ::
    (Ord i) =>
    MealyAutomaton Int i o ->
    [[i]] ->
    ([[o]], CacheState i o, Int)
runCached aut queries = (outs, final, calls)
  where
    ((outs, final), calls) =
        runState (runStateT (mapM (query (CachedSUL (CountingSUL aut))) queries) emptyCacheState) 0

prop_matchesUnderlying :: Mealy Input Output -> [[Input]] -> Property
prop_matchesUnderlying (Mealy aut) queries =
    let (outs, _, _) = runCached aut queries
     in outs === map (snd . walkPure (resetPure aut)) queries

prop_firstQueryGoesThrough :: Mealy Input Output -> Input -> [Input] -> Property
prop_firstQueryGoesThrough (Mealy aut) x xs =
    let (_, final, calls) = runCached aut [x : xs]
     in calls === 1 .&&. queriesSaved final === 0

prop_repeatIsCached :: Mealy Input Output -> Input -> [Input] -> Property
prop_repeatIsCached (Mealy aut) x xs =
    let is = x : xs
        (_, final, calls) = runCached aut [is, is]
     in calls === 1 .&&. queriesSaved final === 1

spec :: Spec
spec = do
    describe "query" $ do
        it "the first time a sequence is queried, it goes to the underlying SUL" $
            property prop_firstQueryGoesThrough

        it "querying the same sequence again is answered from the cache" $
            property prop_repeatIsCached

        it "gives the same outputs as querying the underlying SUL directly" $
            property prop_matchesUnderlying

        it "a prefix of a previous query is also saved, without a further underlying query" $ do
            let (_, final, calls) = runCached toggle [[A, B], [A]]
            calls `shouldBe` 1
            queriesSaved final `shouldBe` 1

        it "a query that was never a prefix of any previous one still reaches the underlying SUL" $ do
            let (_, final, calls) = runCached toggle [[A, B], [B]]
            calls `shouldBe` 2
            queriesSaved final `shouldBe` 0

    describe "cachedExperiment" $
        it "learns the same model as the plain experiment, and saves at least one query" $ do
            let learner = mkLMstar Star
                oracle = either error id (mkWpMethod (WpMethodConfig 2))
                plain = runExperiment (experiment learner oracle) toggle
                (cachedModel, saved) = runExperiment (cachedExperiment learner oracle) toggle
            cachedModel `shouldBe` plain
            saved `shouldSatisfy` (> 0)
