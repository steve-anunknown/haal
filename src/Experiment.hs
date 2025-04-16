module Experiment (
    Experiment,
    Learner (..),
    EquivalenceOracle (..),
) where

import Control.Monad.Reader

import BlackBox (Automaton, SUL)

class EquivalenceOracle or where
    testSuiteSize ::
        ( Automaton aut
        , Ord i
        , Bounded i
        , Enum i
        , Ord s
        , Eq o
        , Bounded s
        , Enum s
        ) =>
        or ->
        aut i o s ->
        Int
    testSuite ::
        ( Automaton aut
        , Ord i
        , Bounded i
        , Enum i
        , Ord s
        , Bounded s
        , Enum s
        , Eq o
        ) =>
        or ->
        aut i o s ->
        [[i]]
    findCex ::
        ( Automaton aut
        , Ord i
        , Bounded i
        , Enum i
        , Ord s
        , Bounded s
        , Enum s
        , Eq o
        , SUL sul
        ) =>
        or ->
        aut i o s ->
        Experiment (sul i o s) (Maybe ([i], [o]))

class Learner l where
    initialize ::
        ( SUL sul
        , Bounded i
        , Enum i
        , Ord i
        , Bounded o
        , Enum o
        , Bounded s
        , Enum s
        , Ord s
        ) =>
        l i o ->
        Experiment (sul i o s) (l i o)
    refine ::
        ( SUL sul
        , Bounded i
        , Enum i
        , Ord i
        , Bounded o
        , Enum o
        , Bounded s
        , Enum s
        , Ord s
        ) =>
        l i o ->
        [i] ->
        Experiment (sul i o s) (l i o)
    learn ::
        ( SUL sul
        , Automaton aut
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
        Experiment (sul i o s) (l i o, aut i o s)

type Experiment sul result = Reader sul result

--
-- experiment ::
--     (SUL sul) =>
--     Learner sul i o sid -> -- learner
--     Teacher sul i o sid -> -- teacher
--     Refiner sul i o sid -> -- refiner
--     Experiment (sul i o sid) (MealyAutomaton i o sid)
-- experiment learner teacher refine = do
--     h <- learner
--     cex <- teacher h
--     case cex of
--         Nothing -> return h
--         Just ce -> refine h ce >>= \h' -> experiment (return h') teacher refine
