{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | This module exports the basic types, classes and functions that are required to
easily construct and configure learning experiments.
-}
module Haal.Experiment (
    Experiment,
    ExperimentT,
    Learner (..),
    EquivalenceOracle (..),
    Statistics (..),
    experiment,
    runExperiment,
    pairwiseWalk,
    execute,
    findCex,
    runExperimentT,
) where

import Control.Monad.Reader (
    MonadReader (ask),
    MonadTrans (lift),
    Reader,
    ReaderT (runReaderT),
    runReader,
 )

import Control.Monad.Identity
import Haal.BlackBox

{- | The 'EquivalenceOracle' type class defines the interface for equivalence oracles.
Instances of this class should provide methods to generate a test suite
-}
class EquivalenceOracle or where
    testSuite ::
        ( Automaton aut s i o
        , FiniteOrd i
        , FiniteOrd s
        , Eq o
        ) =>
        or ->
        aut s i o ->
        (or, [[i]])

{- | The 'Learner' type class defines the interface for learning algorithms.
Instances of this class should provide methods to initialize the learner,
refine the learner with a counterexample, and learn an automaton. The type @l@
determines the type of automaton @aut@ that is learned.
-}
class Learner l aut s | l -> aut s where
    initialize ::
        ( SUL sul m i o
        , FiniteOrd i
        , Finite o
        ) =>
        l i o ->
        ExperimentT (sul i o) m (l i o)
    refine ::
        ( SUL sul m i o
        , FiniteOrd i
        , Finite o
        ) =>
        l i o ->
        [i] ->
        ExperimentT (sul i o) m (l i o)
    learn ::
        ( SUL sul m i o
        , Automaton aut s i o
        , FiniteOrd i
        , FiniteOrd s
        , FiniteEq o
        ) =>
        l i o ->
        ExperimentT (sul i o) m (l i o, aut s i o)

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

{- | The 'Statistics' data type is parameterized by the type of model being learned
and the state, input and output types of the model. Its purpose is to keep track of
different experimental stats. For the time being, only the number of rounds 'statsRounds',
the counterexamples 'statsCexs' and the intermediate hypotheses 'statsHyps' are being kept
track of.
-}
data Statistics aut s i o = Statistics
    { statsRounds :: Int
    , statsCexs :: [[i]]
    , statsHyps :: [aut s i o]
    }
    deriving (Show)

-- | Empty 'Statistics' value.
mkStats :: Statistics aut s i o
mkStats = Statistics 0 [] []

{- | The 'experiment' function returns an 'Experiment' that can be run with
the 'runExperiment' function. It takes a learner and an equivalence oracle
and then requires a system under learning (SUL) to run the experiment.
-}
experiment ::
    ( SUL sul m i o
    , Automaton aut s i o
    , Learner learner aut s
    , EquivalenceOracle oracle
    , FiniteOrd i
    , FiniteOrd s
    , FiniteEq o
    ) =>
    learner i o ->
    oracle ->
    ExperimentT (sul i o) m (aut s i o, Statistics aut s i o)
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
    inner initializedLearner oracle mkStats

-- | The 'execute' function executes the test suite of an oracle, given a SUL and an automaton.
execute ::
    ( SUL sul m i o
    , Automaton aut s i o
    , Ord i
    , Eq o
    ) =>
    sul i o ->
    aut s i o ->
    [[i]] ->
    m ([i], [o])
execute _ _ [] = return ([], [])
execute theSul theAut (s : ss) = do
    continue <- pairwiseWalk theSul theAut s
    if continue
        then execute theSul theAut ss
        else do
            (_, out) <- walk theSul s
            return (s, out)

{- | The 'pairwiseWalk' function executes a test case on both the SUL and the automaton
simultaneously, checking if the outputs are the same.
-}
pairwiseWalk ::
    ( SUL sul m i o
    , Automaton aut s i o
    , Ord i
    , Eq o
    ) =>
    sul i o ->
    aut s i o ->
    [i] ->
    m Bool
pairwiseWalk _ _ [] = return True
pairwiseWalk theSul theAut (s : ss) = do
    (sul', out1) <- step theSul s
    let (aut', out2) = runIdentity (step theAut s)
    rest <- pairwiseWalk sul' aut' ss
    return $ out1 == out2 && rest

{- | The 'findCex' function executes the test suite of each oracle to the automaton
and SUL.
-}
findCex ::
    ( SUL sul m i o
    , Automaton aut s i o
    , EquivalenceOracle or
    , FiniteOrd i
    , FiniteOrd s
    , Eq o
    ) =>
    or ->
    aut s i o ->
    ExperimentT (sul i o) m (or, ([i], [o]))
findCex oracle aut = do
    sul <- ask
    let (oracle', theSuite) = testSuite oracle aut
    (cin, cout) <- lift $ execute sul aut theSuite
    return (oracle', (cin, cout))
