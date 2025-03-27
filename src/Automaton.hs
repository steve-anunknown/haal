module Automaton
  ( Automaton (..),
  )
where

import qualified Data.Map as Map

class Automaton a where
  step :: (Ord i, Ord s) => a i o s -> i -> (a i o s, o)
  walk :: (Ord i, Ord s) => a i o s -> [i] -> (a i o s, [o])
  current :: a i o s -> s
  transitions :: a i o s -> Map.Map (s, i) (s, o)
