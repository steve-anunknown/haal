{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{- | This module exports the basic types, classes and functions that are required to
easily construct and configure learning experiments.
-}
module Haal.Experiment (
    Experiment,
    ExperimentT,
    experiment,
    findCex,
    runExperiment,
    runExperimentT,
    experimentWith,
    measuredExperiment,
) where

import Control.Monad.Reader (
    MonadReader (ask),
    MonadTrans (lift),
    Reader,
    ReaderT (runReaderT),
    runReader,
    withReaderT,
 )
import Control.Monad.State (StateT, modify', runStateT)

import Control.Foldl (Fold (..))
import Control.Monad.Identity
import qualified Data.Map as Map
import Haal.BlackBox
import Haal.EquivalenceOracle
import Haal.Learning
import Haal.Statistics (Event (..), Phase (Learning, Testing))

{- | The 'ExperimentT' type is a monad transformer that allows for
running experiments in a reader monad. This may prove useful for
learning real systems, which requires IO.
-}
type ExperimentT sul m result = ReaderT sul m result

{- | The 'Experiment' type is a type alias for the 'ExperimentT' type
with the 'Identity' monad. This allows for running pure experiments.
-}
type Experiment sul result = ExperimentT sul Identity result

{- | The 'runExperimentT' function runs an experiment in the 'ExperimentT' monad.
It is just an alias for 'runReaderT'.
-}
runExperimentT :: ReaderT r m a -> r -> m a
runExperimentT = runReaderT

{- | The 'runExperiment' function runs an experiment in the 'Experiment' monad.
It is just an alias for 'runReader'.
-}
runExperiment :: Reader r a -> r -> a
runExperiment = runReader

{- | The learning loop, reporting the events it sees itself (phases,
hypotheses, counterexamples) to the given function. It can't observe
membership queries.
-}
loop ::
    ( EquivalenceOracle oracle
    , Learner learner aut
    , FiniteOrd i
    , FiniteOrd o
    , Automaton aut Int
    , SUL sul m
    ) =>
    (Event aut i o -> m ()) ->
    learner i o ->
    oracle ->
    ExperimentT (sul i o) m (aut Int i o)
loop emit learner oracle = do
    lift (emit (PhaseChanged Learning))
    initializedLearner <- initialize learner
    let inner le orc = do
            (learner', aut) <- learn le
            lift (emit (Hypothesis aut))
            lift (emit (PhaseChanged Testing))
            (oracle', cex) <- findCex orc aut
            case cex of
                ([], []) -> return aut
                (ce, _) -> do
                    lift (emit (Counterexample ce))
                    lift (emit (PhaseChanged Learning))
                    refinedLearner <- refine learner' ce
                    inner refinedLearner oracle'
    inner initializedLearner oracle

{- | 'Observed' is going to be used by the experiments. It wraps the SUL
and reports the queries to the experiment loop.
-}
data Observed m sul i o = Observed (sul i o) ([i] -> [o] -> m ())

instance (SUL sul m) => SUL (Observed m sul) m where
    step :: Observed m sul i o -> i -> m (Observed m sul i o, o)
    step (Observed sul report) input = do
        (sul', output) <- step sul input
        return (Observed sul' report, output)
    reset :: Observed m sul i o -> m (Observed m sul i o)
    reset (Observed sul report) = (`Observed` report) <$> reset sul
    query :: Observed m sul i o -> [i] -> m [o]
    query (Observed sul report) is = do
        os <- query sul is
        report is os
        return os

{- | The 'experimentWith' function is 'experiment', reporting every 'Event' to
the given function, in the experiment's monad: the phases ('PhaseChanged'), every
query sent to the SUL ('Queried'), every hypothesis ('Hypothesis') and every
counterexample ('Counterexample'). The experiment starts in the 'Learning'
phase, enters 'Testing' before each hypothesis is validated, and goes back to
'Learning' after each counterexample. For statistics, 'measuredExperiment' is
usually more convenient.
-}
experimentWith ::
    ( EquivalenceOracle oracle
    , Learner learner aut
    , FiniteOrd i
    , FiniteOrd o
    , Automaton aut Int
    , SUL sul m
    ) =>
    (Event aut i o -> m ()) ->
    learner i o ->
    oracle ->
    ExperimentT (sul i o) m (aut Int i o)
experimentWith emit learner oracle =
    withReaderT (\sul -> Observed sul (\is os -> emit (Queried is os))) (loop emit learner oracle)

-- | A SUL lifted into 'StateT', so that 'measuredExperiment' can keep its state there.
newtype Lifted sul i o = Lifted (sul i o)

instance (SUL sul m) => SUL (Lifted sul) (StateT x m) where
    step :: Lifted sul i o -> i -> StateT x m (Lifted sul i o, o)
    step (Lifted sul) input = do
        (sul', output) <- lift (step sul input)
        return (Lifted sul', output)
    reset :: Lifted sul i o -> StateT x m (Lifted sul i o)
    reset (Lifted sul) = Lifted <$> lift (reset sul)
    query :: Lifted sul i o -> [i] -> StateT x m [o]
    query (Lifted sul) = lift . query sul

{- | The 'measuredExperiment' function is 'experiment', also returning a
statistic: a 'Control.Foldl.Fold' over the experiment's events (see
"Haal.Statistics"). 'Haal.Statistics.statistics' measures the usual ones;
combine it with folds of your own through the 'Applicative' instance of 'Fold':

> runExperiment (measuredExperiment statistics learner oracle) sul
> runExperiment (measuredExperiment ((,) <$> statistics <*> myFold) learner oracle) sul
-}
measuredExperiment ::
    ( EquivalenceOracle oracle
    , Learner learner aut
    , FiniteOrd i
    , FiniteOrd o
    , Automaton aut Int
    , SUL sul m
    ) =>
    Fold (Event aut i o) r ->
    learner i o ->
    oracle ->
    ExperimentT (sul i o) m (aut Int i o, r)
measuredExperiment (Fold stepFold x0 done) learner oracle = do
    sul <- ask
    let measured = runReaderT (experimentWith (\e -> modify' (`stepFold` e)) learner oracle) (Lifted sul)
    (model, x) <- lift (runStateT measured x0)
    return (model, done x)

{- | The 'experiment' function returns an 'Experiment' that can be run with
the 'runExperiment' function. It takes a learner and an equivalence oracle
and then requires a system under learning (SUL) to run the experiment.
-}
experiment ::
    ( EquivalenceOracle oracle
    , Learner learner aut
    , FiniteOrd i
    , FiniteOrd o
    , Automaton aut Int
    , SUL sul m
    ) =>
    learner i o ->
    oracle ->
    ExperimentT (sul i o) m (aut Int i o)
experiment = loop (\_ -> return ())

data LRUCache i o = LRUCache
    { mapping :: Map.Map (Int, [i]) [o]
    , capacity :: Int
    }

mkLRUCache capacity = LRUCache Map.empty capacity

insertToCache cache input output =
    if input `Map.member` (mapping cache)
        then cache
        else undefined

-- | A SUL lifted into 'StateT', so that 'measuredExperiment' can keep its state there.
newtype Cached sul i o = Cached (sul i o)

cachedExperiment = undefined
