{-# LANGUAGE RankNTypes #-}
module Agda.BlackBox where

class SUL sul where
    step :: forall i o . sul i o -> i -> (sul i o, o)
    reset :: forall i o . sul i o -> sul i o

helpwalk :: SUL sul => sul i o -> [i] -> [o] -> (sul i o, [o])
helpwalk system [] outputs = (system, reverse outputs)
helpwalk system (input : rest) outputs
  = helpwalk system rest (snd (step system input) : outputs)

walk :: SUL sul => sul i o -> [i] -> (sul i o, [o])
walk system inputs = helpwalk system inputs []

