{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

Statistics are not built into the experiment. They live in the monad the
experiment runs in, and come from two places:

* SUL wrappers, such as 'Counted', which see every query sent to the system;
* hooks ('Hook'), which the experiment calls at fixed moments of the learning
  loop, so they see what only the loop knows: phases, hypotheses and
  counterexamples.

A wrapper and its hooks run in the same monad, so they can share state. For
example, 'countingHooks' tells 'Counted' which phase the experiment is in, so it
can count the queries sent while constructing hypotheses separately from those
sent while validating them. Users can define their own statistics the same way.
-}
module Haal.Statistics (
    -- * Experiment hooks
    Phase (..),
    Hook (..),
    noHooks,
    liftHook,

    -- * Counting queries and symbols
    Tally (..),
    Counts (..),
    Counted (..),
    initialCounts,
    total,
    countingHooks,

    -- * Hypotheses and counterexamples
    History (..),
    emptyHistory,
    rounds,
    historyHooks,
)
where

import Control.Monad.State (MonadTrans (lift), StateT, modify')
import Haal.BlackBox (SUL (..))

-- | The phase of a learning experiment.
data Phase
    = -- | The learner is constructing or refining a hypothesis.
      Learning
    | -- | The oracle is validating a hypothesis. This is the equivalence query,
      -- approximated by conformance testing, which sends test cases to the SUL
      -- as membership queries.
      Testing
    deriving (Show, Eq)

{- | Functions the experiment calls at fixed moments of the learning loop (see
'Haal.Experiment.experimentWith'). Each one runs in the experiment's monad, so it
can update state there.
-}
data Hook m aut i o = Hook
    { onPhase :: Phase -> m ()
    -- ^ Called when the experiment enters a phase.
    , onHypothesis :: aut Int i o -> m ()
    -- ^ Called with every hypothesis, including the final one.
    , onCounterexample :: [i] -> m ()
    -- ^ Called with every counterexample.
    }

-- | Hooks that do nothing.
noHooks :: (Applicative m) => Hook m aut i o
noHooks = Hook{onPhase = \_ -> pure (), onHypothesis = \_ -> pure (), onCounterexample = \_ -> pure ()}

-- | Combining hooks runs both, left first.
instance (Applicative m) => Semigroup (Hook m aut i o) where
    h1 <> h2 =
        Hook
            { onPhase = \p -> onPhase h1 p *> onPhase h2 p
            , onHypothesis = \h -> onHypothesis h1 h *> onHypothesis h2 h
            , onCounterexample = \c -> onCounterexample h1 c *> onCounterexample h2 c
            }

instance (Applicative m) => Monoid (Hook m aut i o) where
    mempty = noHooks

{- | Run hooks one monad transformer layer further out. This combines hooks that
keep their state in different layers, e.g. for an experiment running in
@StateT Counts (StateT (History aut i o) m)@:

> countingHooks <> liftHook historyHooks
-}
liftHook :: (MonadTrans t, Monad m) => Hook m aut i o -> Hook (t m) aut i o
liftHook h =
    Hook
        { onPhase = lift . onPhase h
        , onHypothesis = lift . onHypothesis h
        , onCounterexample = lift . onCounterexample h
        }

{- | The number of queries and symbols sent to a SUL. The fields are strict,
because an experiment sends a great number of queries.
-}
data Tally = Tally {queries :: !Int, symbols :: !Int} deriving (Show, Eq)

{-@ data Tally = Tally {queries :: Nat, symbols :: Nat} @-}

-- | Counts per phase, and the phase the experiment is currently in.
data Counts = Counts
    { phase :: !Phase
    -- ^ The current phase; 'countingHooks' keeps it up to date.
    , learning :: !Tally
    -- ^ Membership queries during hypothesis construction
    , testing :: !Tally
    -- ^ Membership queries during hypothesis validation
    }
    deriving (Show, Eq)

{-@ data Counts = Counts {phase :: Phase, learning :: Tally, testing :: Tally} @-}

-- | No queries counted yet; the experiment starts in the 'Learning' phase.
initialCounts :: Counts
initialCounts = Counts Learning (Tally 0 0) (Tally 0 0)

-- | Queries and symbols of both phases together.

{-@ total :: c:Counts -> {t:Tally | queries t == queries (learning c) + queries (testing c)
                                 && symbols t == symbols (learning c) + symbols (testing c)} @-}
total :: Counts -> Tally
total c = Tally (queries l + queries t) (symbols l + symbols t)
  where
    l = learning c
    t = testing c

{- | Add queries and symbols to the tally of the current phase. Verified by
LiquidHaskell: the phase does not change, the total grows by exactly the given
amounts, and the tally of the other phase is untouched.
-}

{-@ tick :: q:Nat -> s:Nat -> c:Counts
         -> {r:Counts | phase r == phase c
                     && queries (learning r) + queries (testing r) == queries (learning c) + queries (testing c) + q
                     && symbols (learning r) + symbols (testing r) == symbols (learning c) + symbols (testing c) + s
                     && (phase c == Learning => (queries (testing r) == queries (testing c)
                                                 && symbols (testing r) == symbols (testing c)))
                     && (phase c == Testing  => (queries (learning r) == queries (learning c)
                                                 && symbols (learning r) == symbols (learning c)))} @-}
tick :: Int -> Int -> Counts -> Counts
tick q s c = case phase c of
    Learning -> c{learning = add (learning c)}
    Testing -> c{testing = add (testing c)}
  where
    add (Tally q0 s0) = Tally (q0 + q) (s0 + s)

{- | A SUL wrapper that counts the queries and symbols sent to the SUL it wraps,
per phase. Every query starts with one 'reset', so a reset counts as a query
and a step as a symbol. Run the experiment with 'countingHooks' to split the
counts by phase; without them, everything counts as 'Learning'.
-}
newtype Counted sul i o = Counted (sul i o)

instance (SUL sul m) => SUL (Counted sul) (StateT Counts m) where
    step :: Counted sul i o -> i -> StateT Counts m (Counted sul i o, o)
    step (Counted sul) input = do
        modify' (tick 0 1)
        (sul', output) <- lift (step sul input)
        return (Counted sul', output)
    reset :: Counted sul i o -> StateT Counts m (Counted sul i o)
    reset (Counted sul) = do
        modify' (tick 1 0)
        Counted <$> lift (reset sul)

    -- Overridden so that an efficient 'query' of the wrapped SUL is still used.
    query :: Counted sul i o -> [i] -> StateT Counts m [o]
    query (Counted sul) inputs = do
        modify' (tick 1 (length inputs))
        lift (query sul inputs)

-- | Hooks that keep the phase in 'Counts' up to date, for use with 'Counted'.
countingHooks :: (Monad m) => Hook (StateT Counts m) aut i o
countingHooks = noHooks{onPhase = \p -> modify' (\c -> c{phase = p})}

{- | The hypotheses and counterexamples of an experiment, most recent first.
Unlike the @Statistics@ record of haal 0.6, the hypotheses include the final
one.
-}
data History aut i o = History
    { hypotheses :: [aut Int i o]
    , counterexamples :: [[i]]
    }

-- | No hypotheses or counterexamples yet.
emptyHistory :: History aut i o
emptyHistory = History [] []

-- | The number of rounds, i.e. of counterexamples found.
rounds :: History aut i o -> Int
rounds = length . counterexamples

-- | Hooks that record every hypothesis and every counterexample in 'History'.
historyHooks :: (Monad m) => Hook (StateT (History aut i o) m) aut i o
historyHooks =
    noHooks
        { onHypothesis = \h -> modify' (\r -> r{hypotheses = h : hypotheses r})
        , onCounterexample = \c -> modify' (\r -> r{counterexamples = c : counterexamples r})
        }
