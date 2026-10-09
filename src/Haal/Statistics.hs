{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef LIQUID
-- GHC unboxes the strict Int fields of 'Tally' when optimising, which
-- LiquidHaskell cannot match against the data refinement, so verification
-- builds keep them boxed. Remove once this is fixed upstream:
-- https://github.com/ucsd-progsys/liquidhaskell/issues/2629
{-# OPTIONS_GHC -fplugin=LiquidHaskell
                -fplugin-opt=LiquidHaskell:--prune-unsorted
                -fno-unbox-small-strict-fields #-}
#endif

{- | Statistics for learning experiments.

An experiment emits an 'Event' at every step that matters for statistics: when
it changes 'Phase', for every query sent to the SUL, for every hypothesis and for
every counterexample. A statistic is a fold over these events, a
'Control.Foldl.Fold' from the @foldl@ package. Folds combine with their
'Applicative' instance into a single fold that still runs in one pass, so any
number of statistics can be measured at once:

> (model, stats) = runExperiment (measuredExperiment statistics learner oracle) sul

'statistics' measures what most experiments report: the membership queries and
symbols sent while constructing hypotheses and while validating them, and every
hypothesis and counterexample. Combine it with folds of your own:

> runExperiment (measuredExperiment ((,) <$> statistics <*> myFold) learner oracle) sul

See 'Haal.Experiment.measuredExperiment' and 'Haal.Experiment.experimentWith'.
Users define their own statistics as folds, e.g. with 'Control.Foldl.premap'
and 'Control.Foldl.prefilter' over the folds of @foldl@, and can test one by
running it over a list of events with 'Control.Foldl.fold'.
-}
module Haal.Statistics (
    -- * Events
    Phase (..),
    Event (..),

    -- * Statistics
    Statistics (..),
    Tally (..),
    statistics,
    total,
    rounds,
)
where

import qualified Control.Foldl as L

-- | The phase of a learning experiment.
data Phase
    = -- | The learner is constructing or refining a hypothesis.
      Learning
    | -- | The oracle is validating a hypothesis. This is the equivalence query,
      -- approximated by conformance testing, which sends test cases to the SUL
      -- as membership queries.
      Testing
    deriving (Show, Eq)

-- | Something that happened during an experiment.
data Event aut i o
    = -- | The experiment entered a phase.
      PhaseChanged Phase
    | -- | A query was sent to the SUL, with its inputs and outputs.
      Queried [i] [o]
    | -- | The learner produced a hypothesis (every one, including the final one).
      Hypothesis (aut Int i o)
    | -- | The oracle found a counterexample.
      Counterexample [i]

{- | The number of queries and symbols sent to a SUL. The fields are strict,
because an experiment sends a great number of queries.
-}
data Tally = Tally {queries :: !Int, symbols :: !Int} deriving (Show, Eq)

{-@ data Tally = Tally {queries :: Nat, symbols :: Nat} @-}

{- | The statistics of an experiment, measured by 'statistics'. The number of
equivalence queries is the number of hypotheses: each hypothesis is validated
once.
-}
data Statistics aut i o = Statistics
    { learning :: !Tally
    -- ^ Membership queries during hypothesis construction
    , testing :: !Tally
    -- ^ Membership queries during hypothesis validation
    , hypotheses :: [aut Int i o]
    -- ^ Every hypothesis, including the final one, most recent first
    , counterexamples :: [[i]]
    -- ^ Every counterexample, most recent first
    }

deriving instance (Show (aut Int i o), Show i) => Show (Statistics aut i o)
deriving instance (Eq (aut Int i o), Eq i) => Eq (Statistics aut i o)

{-@ data Statistics aut i o = Statistics
      { learning        :: Tally
      , testing         :: Tally
      , hypotheses      :: [aut Int i o]
      , counterexamples :: [[i]]
      } @-}

-- | Queries and symbols of both phases together.

{-@ total :: s:Statistics aut i o -> {t:Tally | queries t == queries (learning s) + queries (testing s)
                                             && symbols t == symbols (learning s) + symbols (testing s)} @-}
total :: Statistics aut i o -> Tally
total s = Tally (queries l + queries t) (symbols l + symbols t)
  where
    l = learning s
    t = testing s

-- | The number of rounds, i.e. of counterexamples found.
rounds :: Statistics aut i o -> Int
rounds = length . counterexamples

{- | Add queries and symbols to the tally of a phase. Verified by LiquidHaskell:
the total grows by exactly the given amounts, and the tally of the other phase
is untouched.
-}

{-@ tick :: p:Phase -> q:Nat -> n:Nat -> s:Statistics aut i o
         -> {r:Statistics aut i o | queries (learning r) + queries (testing r) == queries (learning s) + queries (testing s) + q
                                 && symbols (learning r) + symbols (testing r) == symbols (learning s) + symbols (testing s) + n
                                 && (p == Learning => (queries (testing r) == queries (testing s)
                                                       && symbols (testing r) == symbols (testing s)))
                                 && (p == Testing  => (queries (learning r) == queries (learning s)
                                                       && symbols (learning r) == symbols (learning s)))} @-}
tick :: Phase -> Int -> Int -> Statistics aut i o -> Statistics aut i o
tick p q n s = case p of
    Learning -> s{learning = add (learning s)}
    Testing -> s{testing = add (testing s)}
  where
    add (Tally q0 n0) = Tally (q0 + q) (n0 + n)

-- | The state of 'statistics': the current phase and the statistics so far.
data Acc aut i o = Acc !Phase !(Statistics aut i o)

{- | Measure the 'Statistics' of an experiment: the membership queries and
symbols sent in each phase, and every hypothesis and counterexample.
-}
statistics :: L.Fold (Event aut i o) (Statistics aut i o)
statistics = L.Fold step (Acc Learning (Statistics (Tally 0 0) (Tally 0 0) [] [])) (\(Acc _ s) -> s)
  where
    step (Acc _ s) (PhaseChanged p) = Acc p s
    step (Acc p s) (Queried is _) = Acc p (tick p 1 (length is) s)
    step (Acc p s) (Hypothesis aut) = Acc p s{hypotheses = aut : hypotheses s}
    step (Acc p s) (Counterexample cex) = Acc p s{counterexamples = cex : counterexamples s}
