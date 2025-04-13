module EquivalenceOracle (
    EquivalenceOracle (..),
)
where

import BlackBox (Automaton, SUL)
import Data.Data (Data)

class EquivalenceOracle or where
    testSuiteSize ::
        ( Automaton aut
        , Ord i
        , Data i
        , Ord s
        , Data s
        , Eq o
        , Bounded s
        ) =>
        or ->
        aut i o s ->
        Int
    testSuite ::
        ( Automaton aut
        , Ord i
        , Data i
        , Ord s
        , Data s
        , Eq o
        , Bounded s
        ) =>
        or ->
        aut i o s ->
        [[i]]
    findCex ::
        ( Automaton aut
        , Ord i
        , Data i
        , Ord s
        , Data s
        , Eq o
        , Bounded s
        , SUL sul
        ) =>
        or ->
        aut i o s ->
        sul i o s ->
        Maybe ([i], [o])
