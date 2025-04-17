{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Experiment (
    Experiment,
    Learner (..),
    EquivalenceOracle (..),
    experiment,
) where

import Control.Monad.Reader

import BlackBox (Automaton, SUL)

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

type Experiment sul result = Reader sul result

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
