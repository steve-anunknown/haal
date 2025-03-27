module BlackBox
  ( Automaton (..),
    SUL (..),
    BlackBox (..),
  )
where

import qualified Data.Map as Map

class BlackBox a where
  step :: (Ord i, Ord s) => a i o s -> i -> (a i o s, o)
  walk :: (Ord i, Ord s) => a i o s -> [i] -> (a i o s, [o])
  current :: a i o s -> s

class (BlackBox a) => Automaton a where
  transitions :: a i o s -> Map.Map (s, i) (s, o)

class (BlackBox a) => SUL a where
  reset :: (Bounded s) => a i o s -> a i o s
