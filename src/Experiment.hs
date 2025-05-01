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
    pairwiseWalk,
    execute,
    findCex,
) where

import Control.Monad.Reader

import BlackBox (Automaton, Finite, FiniteEq, FiniteOrd, SUL, step, walk)
import Control.Monad.Identity

{- | The 'EquivalenceOracle' type class defines the interface for equivalence oracles.
Instances of this class should provide methods to generate a test suite
-}
class EquivalenceOracle or where
    testSuite ::
        ( Automaton aut s
        , FiniteOrd i
        , FiniteOrd s
        , Eq o
        ) =>
        or ->
        aut i o ->
        (or, [[i]])

{- | The 'Learner' type class defines the interface for learning algorithms.
Instances of this class should provide methods to initialize the learner,
refine the learner with a counterexample, and learn an automaton. The type 'l'
determines the type of automaton 'aut' that is learned.
-}
class Learner l aut | l -> aut where
    initialize ::
        ( SUL sul
        , Automaton aut s
        , FiniteOrd i
        , Finite o
        ) =>
        l i o ->
        Experiment (sul i o) (l i o)
    refine ::
        ( SUL sul
        , FiniteOrd i
        , Finite o
        ) =>
        l i o ->
        [i] ->
        Experiment (sul i o) (l i o)
    learn ::
        ( SUL sul
        , Automaton aut s
        , FiniteOrd i
        , FiniteOrd s
        , FiniteEq o
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

data Statistics aut i o = Statistics
    { statsRounds :: Int
    , statsCexs :: [[i]]
    , statsHyps :: [aut i o]
    } deriving(Show)

statsEmpty :: Statistics aut i o
statsEmpty = Statistics 0 [] []

{- | The 'experiment' function returns an 'Experiment' that can be run with
the 'runExperiment' function. It takes a learner and an equivalence oracle
and then requires a system under learning (SUL) to run the experiment.
-}
experiment ::
    ( SUL sul
    , Automaton aut s
    , Learner learner aut
    , EquivalenceOracle oracle
    , FiniteOrd i
    , FiniteOrd s
    , FiniteEq o
    ) =>
    learner i o ->
    oracle ->
    Experiment (sul i o) (aut i o, Statistics aut i o)
experiment learner oracle = do
    initializedLearner <- initialize learner
    let inner le orc stats = do
            (learner', aut) <- learn le
            (oracle', cex) <- findCex orc aut
            case cex of
                ([], []) -> return (aut, stats)
                (ce, _) -> do
                    refinedLearner <- refine learner' ce
                    let rounds = statsRounds stats
                        cexs = statsCexs stats
                        hyps = statsHyps stats
                        stats' = Statistics (rounds + 1) (ce : cexs) (aut : hyps)
                    inner refinedLearner oracle' stats'
    inner initializedLearner oracle statsEmpty

-- | The 'execute' function executes the test suite of an oracle, given a SUL and an automaton.
execute ::
    ( SUL sul
    , Automaton aut s
    , Ord i
    , Eq o
    ) =>
    sul i o ->
    aut i o ->
    [[i]] ->
    ([i], [o])
execute _ _ [] = ([], [])
execute theSul theAut (s : ss) =
    if continue
        then execute theSul theAut ss
        else (s, snd $ walk theSul s)
  where
    continue = pairwiseWalk theSul theAut s

{- | The 'pairwiseWalk' function executes a test case on both the SUL and the automaton
simultaneously, checking if the outputs are the same.
-}
pairwiseWalk ::
    ( SUL sul
    , Automaton aut s
    , Ord i
    , Eq o
    ) =>
    sul i o ->
    aut i o ->
    [i] ->
    Bool
pairwiseWalk _ _ [] = True
pairwiseWalk theSul theAut (s : ss) = (out1 == out2) && pairwiseWalk sul' aut' ss
  where
    (sul', out1) = step theSul s
    (aut', out2) = step theAut s

{- | The 'findCex' function executes the test suite of each oracle to the automaton
and SUL.
-}
findCex ::
    ( SUL sul
    , Automaton aut s
    , EquivalenceOracle or
    , FiniteOrd i
    , FiniteOrd s
    , Eq o
    ) =>
    or ->
    aut i o ->
    Experiment (sul i o) (or, ([i], [o]))
findCex oracle aut = do
    sul <- ask
    let (oracle', theSuite) = testSuite oracle aut
    return (oracle', execute sul aut theSuite)
