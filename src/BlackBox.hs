{-# LANGUAGE FunctionalDependencies #-}

{- | This module defines the BlackBox type class as well as the Automaton and SUL
sub classes.
-}
module BlackBox (
    Automaton (..),
    SUL (..),
    StateID,
)
where

import qualified Data.Map as Map
import qualified Data.Set as Set

{- | The 'StateID' type is an alias for an integer that represents the state of the automaton.
 - It is used as a default type for the state of learned automata.
-}
type StateID = Int

{- | The 'SUL' type class defines the basic interface for a black box automaton.
It provides methods to step through the automaton, walk through a list of inputs,
and retrieve the current state.
-}
class SUL sul where
    step :: (Ord i) => sul i o -> i -> (sul i o, o)
    walk :: (Ord i) => sul i o -> [i] -> (sul i o, [o])
    reset :: sul i o -> sul i o
    inputs :: (Ord i, Bounded i, Enum i) => sul i o -> Set.Set i
    outputs :: (Ord o, Bounded o, Enum o) => sul i o -> Set.Set o

{- | The 'Automaton' type class extends the 'SUL' type class and adds
support for automata operations.
-}
class (SUL aut) => Automaton aut st | aut -> st where
    transitions :: (Bounded st, Bounded i, Enum st, Enum i, Ord st, Ord i) => aut i o -> Map.Map (st, i) (st, o)
    states :: (Ord st, Bounded st, Enum st) => aut i o -> Set.Set st
    current :: aut i o -> st
    accessSequences :: (Ord i, Bounded i, Enum i, Ord st) => aut i o -> Map.Map st [i]
    localCharacterizingSet :: (Bounded i, Enum i, Bounded st, Enum st, Ord i, Ord st, Eq o) => aut i o -> st -> Set.Set [i]
    globalCharacterizingSet :: (Bounded i, Enum i, Ord i, Bounded st, Enum st, Ord st, Eq o) => aut i o -> Set.Set [i]
