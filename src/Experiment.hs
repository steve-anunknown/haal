module Experiment (experiment) where

experiment :: p1 -> (p1 -> t1 -> Maybe t2) -> (p1 -> p2 -> t1) -> t1
experiment sul teacher learner = go emptyState
  where
    emptyState = undefined
    updateState = undefined

    go state =
        let
            h = learner sul state
            cex = teacher sul h
         in
            case cex of
                Nothing -> h
                Just ce -> go (updateState state ce)

