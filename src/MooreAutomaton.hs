{-# LANGUAGE ScopedTypeVariables #-}

module MooreAutomaton (
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
import qualified Data.Data as Data
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

data MooreAutomaton input output state = MooreAutomaton
    { mooreDelta :: state -> input -> state
    , mooreLambda :: state -> output
    , mooreInitialS :: state
    , mooreCurrentS :: state
    }

mkMooreAutomaton :: (s -> i -> s) -> (s -> o) -> s -> MooreAutomaton i o s
mkMooreAutomaton delta lambda initial =
    MooreAutomaton
        { mooreDelta = delta
        , mooreLambda = lambda
        , mooreInitialS = initial
        , mooreCurrentS = initial
        }

-- | Convenience function that updates the current state 'mooreCurrentS' of the automaton.
mooreUpdateState :: MooreAutomaton i o s -> s -> MooreAutomaton i o s
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
mooreStep :: MooreAutomaton i o s -> i -> (MooreAutomaton i o s, o)
mooreStep m i = (mooreUpdateState m nextState, output)
  where
    nextState = mooreDelta m (mooreCurrentS m) i
    output = mooreLambda m (mooreCurrentS m)

{- | Extends the 'mooreStep' function by taking a series of inputs and performing multiple steps in
the automaton, returning a tuple containing the automaton with a modified state as well as the
outputs produced by the transitions.
-}
mooreWalk :: (Traversable t) => MooreAutomaton i o s -> t i -> (MooreAutomaton i o s, t o)
mooreWalk = List.mapAccumL mooreStep

-- | Resets the automaton to its initial state.
mooreReset :: MooreAutomaton i o s -> MooreAutomaton i o s
mooreReset m = mooreUpdateState m (mooreInitialS m)

{- | Returns a map describing the combined behaviour of the 'mooreDelta'
and 'mooreLambda' functions.
-}
mooreTransitions ::
    forall i o s.
    (Data.Data s, Data.Data i, Ord s, Ord i) =>
    MooreAutomaton i o s ->
    Map.Map (s, i) (s, o)
mooreTransitions m = Map.fromList [((s, i), (delta s i, lambda s)) | s <- domainS, i <- domainI]
  where
    delta = mooreDelta m
    lambda = mooreLambda m
    constructorsS = Data.dataTypeConstrs $ Data.dataTypeOf (undefined :: s)
    constructorsI = Data.dataTypeConstrs $ Data.dataTypeOf (undefined :: i)
    domainS = List.map Data.fromConstr constructorsS
    domainI = List.map Data.fromConstr constructorsI

mooreInAlphabet :: forall i o s. (Ord i, Data.Data i) => MooreAutomaton i o s -> Set.Set i
mooreInAlphabet _ = Set.fromList (List.map Data.fromConstr (Data.dataTypeConstrs $ Data.dataTypeOf (undefined :: i)) :: [i])

mooreOutAlphabet :: forall i o s. (Ord o, Data.Data o) => MooreAutomaton i o s -> Set.Set o
mooreOutAlphabet _ = Set.fromList (List.map Data.fromConstr (Data.dataTypeConstrs $ Data.dataTypeOf (undefined :: o)) :: [o])

mooreStates ::
    forall i o s.
    (Ord s, Data.Data s) =>
    MooreAutomaton i o s ->
    Set.Set s
mooreStates _ = Set.fromList (List.map Data.fromConstr (Data.dataTypeConstrs $ Data.dataTypeOf (undefined :: s)) :: [s])

instance BlackBox.BlackBox MooreAutomaton where
    step = mooreStep
    walk = mooreWalk
    inputs = mooreInAlphabet
    outputs = mooreOutAlphabet

instance BlackBox.Automaton MooreAutomaton where
    transitions = mooreTransitions
    current = mooreCurrentS
    states = mooreStates
    localCharacterizingSet = error "TODO"
    globalCharacterizingSet = error "TODO"

instance BlackBox.SUL MooreAutomaton where
    reset = mooreReset
