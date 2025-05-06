{-# LANGUAGE RankNTypes #-}
module Agda.BlackBox where

class SUL sul where
    step :: forall i o . sul i o -> i -> (sul i o, o)
    reset :: forall i o . sul i o -> sul i o

inputs :: (Enum i, Bounded i) => sul i o -> [i]
inputs = error "postulate:   (Enum i, Bounded i) => sul i o -> [i]"

outputs :: (Enum o, Bounded o) => sul i o -> [o]
outputs
  = error "postulate:   (Enum o, Bounded o) => sul i o -> [o]"

walk' :: SUL sul => sul i o -> [i] -> [o] -> (sul i o, [o])
walk' system [] outputs = (system, reverse outputs)
walk' system (input : rest) outputs
  = walk' system rest (snd (step system input) : outputs)

walk :: SUL sul => sul i o -> [i] -> (sul i o, [o])
walk system is = walk' system is []

class SUL aut => Automaton aut st where
    current :: forall i o . aut i o -> st
    update :: forall i o . aut i o -> st -> aut i o

initial :: Automaton aut st => aut i o -> st
initial = current . reset

