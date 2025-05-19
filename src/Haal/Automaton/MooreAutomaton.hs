{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements a Moore automaton.
module Haal.Automaton.MooreAutomaton (
    MooreAutomaton,
    mkMooreAutomaton,
    mooreStep,
    mooreTransitions,
)
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Haal.BlackBox

data MooreAutomaton state input output = MooreAutomaton
    { mooreDelta :: state -> input -> state
    , mooreLambda :: state -> output
    , mooreInitialS :: state
    , mooreCurrentS :: state
    , mooreStates :: Set.Set state
    }

{- | The 'mkMooreAutomaton' constructor returns a 'MooreAutomaton' by requiring the 'mooreDelta'
function, the 'mooreLambda' function and the initial state 'mooreInitialS'.
-}
mkMooreAutomaton :: (s -> i -> s) -> (s -> o) -> Set.Set s -> s -> MooreAutomaton s i o
mkMooreAutomaton delta lambda sts initS =
    MooreAutomaton
        { mooreDelta = delta
        , mooreLambda = lambda
        , mooreInitialS = initS
        , mooreCurrentS = initS
        , mooreStates = sts
        }

{- | Performs a step in the automaton and returns a tuple containing the automaton with a modified
state as well as the output produced by the transition.
-}
mooreStep :: MooreAutomaton s i o -> i -> (MooreAutomaton s i o, o)
mooreStep m i = (m{mooreCurrentS = nextState}, output)
  where
    nextState = mooreDelta m (mooreCurrentS m) i
    output = mooreLambda m (mooreCurrentS m)

-- | Resets the automaton to its initial state.
mooreReset :: MooreAutomaton s i o -> MooreAutomaton s i o
mooreReset m = m{mooreCurrentS = mooreInitialS m}

instance SUL (MooreAutomaton s) i o where
    step = mooreStep
    reset = mooreReset

{- | Returns a map describing the combined behaviour of the 'mooreDelta'
and 'mooreLambda' functions.
-}
mooreTransitions ::
    forall i o s.
    (FiniteOrd s, FiniteOrd i) =>
    MooreAutomaton s i o ->
    Map.Map (s, i) (s, o)
mooreTransitions m = Map.fromList [((s, i), (delta s i, lambda s)) | s <- domainS, i <- domainI]
  where
    delta = mooreDelta m
    lambda = mooreLambda m
    domainS = Set.toList $ mooreStates m
    domainI = Set.toList $ inputs m

instance Automaton MooreAutomaton s i o where
    transitions = mooreTransitions
    current = mooreCurrentS
    states = mooreStates
    update m s = m{mooreCurrentS = s}

instance
    ( Show i
    , Show o
    , Show s
    , FiniteOrd s
    , FiniteOrd i
    ) =>
    Show (MooreAutomaton s i o)
    where
    show m =
        "{\n\tCurrent State: "
            ++ show currentS
            ++ ",\n\tInitial State: "
            ++ show initialS
            ++ ",\n\tTransitions: "
            ++ show transs
            ++ "\n}"
      where
        transs = mooreTransitions m
        initialS = initial m
        currentS = current m
