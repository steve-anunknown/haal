module EquivalenceOracle (
    EquivalenceOracle (..),
)
where

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
        sul i o s ->
        Maybe ([i], [o])
