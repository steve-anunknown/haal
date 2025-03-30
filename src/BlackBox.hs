{- | This module defines the BlackBox type class as well as the Automaton and SUL
sub classes.
-}
module BlackBox (
    Automaton (..),
    SUL (..),
    BlackBox (..),
)
where

import qualified Data.Data as Data
import qualified Data.Map as Map

{- | The 'BlackBox' type class defines the basic interface for a black box automaton.
It provides methods to step through the automaton, walk through a list of inputs,
and retrieve the current state.
-}
class BlackBox a where
    step :: (Ord i, Ord s) => a i o s -> i -> (a i o s, o)
    walk :: (Ord i, Ord s) => a i o s -> [i] -> (a i o s, [o])
    current :: a i o s -> s

{- | The 'Automaton' type class extends the 'BlackBox' type class and adds a method
to retrieve the transitions of the automaton.
-}
class (BlackBox a) => Automaton a where
    transitions :: (Data.Data s, Data.Data i, Ord s, Ord i) => a i o s -> Map.Map (s, i) (s, o)

{- | The 'SUL' type class extends the 'BlackBox' type class and adds a method
to reset the automaton to its initial state.
-}
class (BlackBox a) => SUL a where
    reset :: (Bounded s) => a i o s -> a i o s
