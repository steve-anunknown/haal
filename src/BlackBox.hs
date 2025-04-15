{- | This module defines the BlackBox type class as well as the Automaton and SUL
sub classes.
-}
module BlackBox (
    Automaton (..),
    SUL (..),
)
where

import qualified Data.Map as Map
import qualified Data.Set as Set

{- | The 'SUL' type class defines the basic interface for a black box automaton.
It provides methods to step through the automaton, walk through a list of inputs,
and retrieve the current state.
-}
class SUL a where
    step :: (Ord i, Ord s) => a i o s -> i -> (a i o s, o)
    walk :: (Ord i, Ord s) => a i o s -> [i] -> (a i o s, [o])
    reset :: (Bounded s) => a i o s -> a i o s
    inputs :: (Ord i, Bounded i, Enum i) => a i o s -> Set.Set i
    outputs :: (Ord o, Bounded o, Enum o) => a i o s -> Set.Set o

{- | The 'Automaton' type class extends the 'SUL' type class and adds
support for automata operations.
-}
class (SUL a) => Automaton a where
    transitions :: (Bounded s, Bounded i, Enum s, Enum i, Ord s, Ord i) => a i o s -> Map.Map (s, i) (s, o)
    states :: (Ord s, Bounded s, Enum s) => a i o s -> Set.Set s
    current :: a i o s -> s
    accessSequences :: (Ord i, Bounded i, Enum i, Ord s) => a i o s -> Map.Map s [i]
    localCharacterizingSet :: (Bounded i, Enum i, Bounded s, Enum s, Ord i, Ord s, Eq o) => a i o s -> s -> Set.Set [i]
    globalCharacterizingSet :: (Bounded i, Enum i, Bounded s, Enum s, Ord i, Ord s, Eq o) => a i o s -> Set.Set [i]
