{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Haal.Learning (
    Learner (..),
)
where

import Control.Monad.Reader (ReaderT)
import Haal.BlackBox

{- | The 'Learner' type class defines the interface for learning algorithms.
Instances of this class should provide methods to initialize the learner,
refine the learner with a counterexample, and learn an automaton. The type @l@
determines the type of automaton @aut@ that is learned.
-}
class Learner l aut | l -> aut where
    initialize ::
        ( SUL sul m
        , FiniteOrd i
        , Finite o
        ) =>
        l i o ->
        ReaderT (sul i o) m (l i o)
    refine ::
        ( SUL sul m
        , FiniteOrd i
        , Finite o
        ) =>
        l i o ->
        [i] ->
        ReaderT (sul i o) m (l i o)
    learn ::
        ( SUL sul m
        , Automaton aut Int
        , FiniteOrd i
        , FiniteOrd o
        ) =>
        l i o ->
        ReaderT (sul i o) m (l i o, aut Int i o)
