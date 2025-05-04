{-# LANGUAGE RankNTypes #-}
module Agda.BlackBox where

class SUL sul where
    step :: forall i o . sul i o -> i -> (sul i o, o)
    reset :: forall i o . sul i o -> sul i o

