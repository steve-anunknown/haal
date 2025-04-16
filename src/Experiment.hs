{-# LANGUAGE FunctionalDependencies #-}

module Experiment (
    Experiment,
    Learner (..),
    EquivalenceOracle (..),
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

--
-- experiment ::
--     (SUL sul) =>
--     Learner sul i oid -> -- learner
--     Teacher sul i oid -> -- teacher
--     Refiner sul i oid -> -- refiner
--     Experiment (sul i oid) (MealyAutomaton i o sid)
-- experiment learner teacher refine = do
--     h <- learner
--     cex <- teacher h
--     case cex of
--         Nothing -> return h
--         Just ce -> refine h ce >>= \h' -> experiment (return h') teacher refine
