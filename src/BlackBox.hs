{- | This module defines the BlackBox type class as well as the Automaton and SUL
sub classes.
-}
module BlackBox (
    Automaton (..),
    SUL (..),
)
where

import Data.Data (Data)
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
    inputs :: (Ord i, Data i) => a i o s -> Set.Set i
    outputs :: (Ord o, Data o) => a i o s -> Set.Set o

{- | The 'Automaton' type class extends the 'SUL' type class and adds
support for automata operations.
-}
class (SUL a) => Automaton a where
    transitions :: (Data s, Data i, Ord s, Ord i) => a i o s -> Map.Map (s, i) (s, o)
    states :: (Ord s, Data s) => a i o s -> Set.Set s
    current :: a i o s -> s
    accessSequences :: (Data i, Ord i, Ord s) => a i o s -> Map.Map s [i]
    localCharacterizingSet :: (Data i, Data s, Ord i, Ord s, Eq o) => a i o s -> s -> Set.Set [i]
    globalCharacterizingSet :: (Data i, Data s, Ord i, Ord s, Eq o) => a i o s -> Set.Set [i]
