{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | This module exports the basic types, classes and functions that are required to
easily construct and configure learning experiments.
-}
module Experiment (
    Experiment,
    ExperimentT,
    Learner (..),
    EquivalenceOracle (..),
    experiment,
    runExperiment,
) where

import Control.Monad.Reader

import BlackBox (Automaton, SUL)
import Control.Monad.Identity

{- | The 'EquivalenceOracle' type class defines the interface for equivalence oracles.
Instances of this class should provide methods to calculate the size of a test suite,
generate a test suite, and search for counterexamples.
-}
class EquivalenceOracle or where
    testSuiteSize ::
        ( Automaton aut s
        , Ord i
        , Bounded i
        , Enum i
        , Ord s
        , Eq o
        , Bounded s
        , Enum s
        ) =>
        or ->
        aut i o ->
        Int
    testSuite ::
        ( Automaton aut s
        , Ord i
        , Bounded i
        , Enum i
        , Ord s
        , Bounded s
        , Enum s
        , Eq o
        ) =>
        or ->
        aut i o ->
        [[i]]
    findCex ::
        ( Automaton aut s
        , SUL sul
        , Ord i
        , Bounded i
        , Enum i
        , Ord s
        , Bounded s
        , Enum s
        , Eq o
        ) =>
        or ->
        aut i o ->
        Experiment (sul i o) (Maybe ([i], [o]))

{- | The 'Learner' type class defines the interface for learning algorithms.
Instances of this class should provide methods to initialize the learner,
refine the learner with a counterexample, and learn an automaton. The type 'l'
determines the type of automaton 'aut' that is learned.
-}
class Learner l aut | l -> aut where
    initialize ::
        ( SUL sul
        , Automaton aut s
        , Bounded i
        , Enum i
        , Ord i
        , Bounded o
        , Enum o
        ) =>
        l i o ->
        Experiment (sul i o) (l i o)
    refine ::
        ( SUL sul
        , Bounded i
        , Enum i
        , Ord i
        , Bounded o
        , Enum o
        ) =>
        l i o ->
        [i] ->
        Experiment (sul i o) (l i o)
    learn ::
        ( SUL sul
        , Automaton aut s
        , Bounded i
        , Enum i
        , Ord i
        , Bounded o
        , Enum o
        , Eq o
        , Bounded s
        , Enum s
        , Ord s
        ) =>
        l i o ->
        Experiment (sul i o) (l i o, aut i o)

{- | The 'ExperimentT' type is a monad transformer that allows for
running experiments in a reader monad. This may prove useful for
learning real systems, which requires IO.
-}
type ExperimentT sul m result = ReaderT sul m result

{- | The 'Experiment' type is a type alias for the 'ExperimentT' type
with the 'Identity' monad. This allows for running pure experiments.
-}
type Experiment sul result = ExperimentT sul Identity result

{- | The 'runExperiment' function runs an experiment in the 'Experiment' monad.
It is just an alias for 'runReader'.
-}
runExperiment :: Experiment r a -> r -> a
runExperiment = runReader

{- | The 'experiment' function returns an 'Experiment' that can be run with
the 'runExperiment' function. It takes a learner and an equivalence oracle
and then requires a system under learning (SUL) to run the experiment.
-}
experiment ::
    ( SUL sul
    , Automaton aut s
    , Learner learner aut
    , EquivalenceOracle oracle
    , Ord i
    , Enum i
    , Enum o
    , Bounded i
    , Bounded o
    , Ord s
    , Enum s
    , Bounded s
    , Eq o
    ) =>
    learner i o ->
    oracle ->
    Experiment (sul i o) (aut i o)
experiment learner oracle = do
    initializedLearner <- initialize learner
    let inner le orc = do
            (learner', aut) <- learn le
            cex <- findCex orc aut
            case cex of
                Nothing -> return aut
                Just (ce, _) -> do
                    refinedLearner <- refine learner' ce
                    inner refinedLearner orc
    inner initializedLearner oracle
