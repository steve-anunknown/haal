module Automaton
  ( Automaton (..),
    Symbol,
    State,
  )
where

import GHC.Natural
import qualified Data.Set as Set

type Symbol = String

type State = Natural

class Automaton a where
  step :: a -> Symbol -> Maybe (a, Symbol)
  walk :: a -> [Symbol] -> Maybe (a, [Symbol])
  state :: a -> State
  inputs :: a -> Set.Set Symbol
  outputs :: a -> Set.Set Symbol
