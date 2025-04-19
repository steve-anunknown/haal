{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements a Moore automaton.
module Automaton.MooreAutomaton (
    mkMooreAutomaton,
    mooreStep,
    mooreWalk,
    mooreTransitions,
    mooreInAlphabet,
    mooreOutAlphabet,
    mooreStates,
)
where

import qualified BlackBox
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

data MooreAutomaton state input output = MooreAutomaton
    { mooreDelta :: state -> input -> state
    , mooreLambda :: state -> output
    , mooreInitialS :: state
    , mooreCurrentS :: state
    }

{- | The 'mkMooreAutomaton' constructor returns a 'MooreAutomaton' by requiring the 'mooreDelta'
function, the 'mooreLambda' function and the initial state 'mooreInitialS'.
-}
mkMooreAutomaton :: (s -> i -> s) -> (s -> o) -> s -> MooreAutomaton s i o
mkMooreAutomaton delta lambda initial =
    MooreAutomaton
        { mooreDelta = delta
        , mooreLambda = lambda
        , mooreInitialS = initial
        , mooreCurrentS = initial
        }

-- | Convenience function that updates the current state 'mooreCurrentS' of the automaton.
mooreUpdateState :: MooreAutomaton s i o -> s -> MooreAutomaton s i o
mooreUpdateState m s =
    MooreAutomaton
        { mooreDelta = delta
        , mooreLambda = lambda
        , mooreInitialS = initial
        , mooreCurrentS = s
        }
  where
    delta = mooreDelta m
    lambda = mooreLambda m
    initial = mooreInitialS m

{- | Performs a step in the automaton and returns a tuple containing the automaton with a modified
state as well as the output produced by the transition.
-}
mooreStep :: MooreAutomaton s i o -> i -> (MooreAutomaton s i o, o)
mooreStep m i = (mooreUpdateState m nextState, output)
  where
    nextState = mooreDelta m (mooreCurrentS m) i
    output = mooreLambda m (mooreCurrentS m)

{- | Extends the 'mooreStep' function by taking a series of inputs and performing multiple steps in
the automaton, returning a tuple containing the automaton with a modified state as well as the
outputs produced by the transitions.
-}
mooreWalk :: (Traversable t) => MooreAutomaton s i o -> t i -> (MooreAutomaton s i o, t o)
mooreWalk = List.mapAccumL mooreStep

-- | Resets the automaton to its initial state.
mooreReset :: MooreAutomaton s i o -> MooreAutomaton s i o
mooreReset m = mooreUpdateState m (mooreInitialS m)

{- | Returns a map describing the combined behaviour of the 'mooreDelta'
and 'mooreLambda' functions.
-}
mooreTransitions ::
    forall i o s.
    (Bounded s, Enum s, Bounded i, Enum i, Ord s, Ord i) =>
    MooreAutomaton s i o ->
    Map.Map (s, i) (s, o)
mooreTransitions m = Map.fromList [((s, i), (delta s i, lambda s)) | s <- domainS, i <- domainI]
  where
    delta = mooreDelta m
    lambda = mooreLambda m
    domainS = [minBound .. maxBound] :: [s]
    domainI = [minBound .. maxBound] :: [i]

-- | Returns the input alphabet of the automaton.
mooreInAlphabet :: forall i o s. (Ord i, Bounded i, Enum i) => MooreAutomaton s i o -> Set.Set i
mooreInAlphabet _ = Set.fromList [minBound .. maxBound] :: Set.Set i

-- | Returns the output alphabet of the automaton.
mooreOutAlphabet :: forall i o s. (Ord o, Bounded o, Enum o) => MooreAutomaton s i o -> Set.Set o
mooreOutAlphabet _ = Set.fromList [minBound .. maxBound] :: Set.Set o

-- | Returns a set of all values of the state alphabet of the automaton.
mooreStates :: forall i o s. (Ord s, Bounded s, Enum s) => MooreAutomaton s i o -> Set.Set s
mooreStates _ = Set.fromList [minBound .. maxBound] :: Set.Set s

instance BlackBox.SUL (MooreAutomaton s) where
    step = mooreStep
    walk = mooreWalk
    reset = mooreReset
    inputs = mooreInAlphabet
    outputs = mooreOutAlphabet

instance BlackBox.Automaton (MooreAutomaton s) s where
    transitions = mooreTransitions
    current = mooreCurrentS
    states = mooreStates
    localCharacterizingSet = error "TODO"
    globalCharacterizingSet = error "TODO"
    accessSequences = error "TODO"
