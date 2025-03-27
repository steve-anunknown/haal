module Model
  ( Model (..),
  )
where

class Model a where
  step :: (Ord i, Ord s) => a i o s -> i -> (a i o s, o)
  walk :: (Ord i, Ord s) => a i o s -> [i] -> (a i o s, [o])
  current :: a i o s -> s
  reset :: (Bounded s) => a i o s -> a i o s
