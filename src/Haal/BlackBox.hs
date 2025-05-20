{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | This module defines the BlackBox type class as well as the Automaton and SUL
sub classes.
-}
module Haal.BlackBox (
    Automaton (..),
    SUL (..),
    StateID,
    Finite,
    FiniteEq,
    FiniteOrd,
    inputs,
    outputs,
    walk,
    initial,
    distinguish,
    accessSequences,
    localCharacterizingSet,
    globalCharacterizingSet,
)
where

import qualified Data.Bifunctor as Bif
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

{- | The 'StateID' type is an alias for an integer that represents the state of the automaton.
 - It is used as a default type for the state of learned automata.
-}
type StateID = Int

{- | The 'SUL' type class defines the basic interface for a black box automaton.
It provides methods to step through the automaton, walk through a list of inputs,
and retrieve the current state.
-}
class SUL sul i o where
    step :: sul i o -> i -> (sul i o, o)
    reset :: sul i o -> sul i o

type Finite i = (Enum i, Bounded i)
type FiniteEq i = (Eq i, Finite i)
type FiniteOrd i = (Ord i, Finite i)

walk :: (SUL sul i o) => sul i o -> [i] -> (sul i o, [o])
walk = List.mapAccumL step

inputs :: (FiniteOrd i) => sul i o -> Set.Set i
inputs _ = Set.fromList [minBound .. maxBound]

outputs :: (FiniteOrd o) => sul i o -> Set.Set o
outputs _ = Set.fromList [minBound .. maxBound]

{- | The 'Automaton' type class extends the 'SUL' type class and adds
support for automata operations.
-}
class (SUL (aut s) i o) => Automaton aut s i o where
    transitions ::
        (FiniteOrd i, FiniteOrd s) =>
        aut s i o ->
        Map.Map (s, i) (s, o)
    states :: (FiniteOrd s) => aut s i o -> Set.Set s
    current :: aut s i o -> s
    update :: aut s i o -> s -> aut s i o

initial :: (Automaton aut s i o) => aut s i o -> s
initial = current . reset

-- | Returns a map containing the shortest sequence to access each reachable state from the initial state.
accessSequences ::
    forall s i o aut.
    (Automaton aut s i o, FiniteOrd i, Ord s) =>
    aut s i o ->
    Map.Map s [i]
accessSequences aut = bfs [(initialSt, [])] (Set.singleton initialSt) (Map.singleton initialSt [])
  where
    alphabet = Set.toList (inputs aut)
    initialSt = initial aut

    bfs :: [(s, [i])] -> Set.Set s -> Map.Map s [i] -> Map.Map s [i]
    bfs [] _ acc = Map.map List.reverse acc
    bfs ((_, prefix) : rest) visited acc =
        bfs (rest ++ newQueue) newVisited newMap
      where
        mo = fst $ walk (reset aut) (reverse prefix)
        successors =
            [ (nextState, input : prefix)
            | input <- alphabet
            , let nextState = current . fst $ step mo input
            , nextState `Set.notMember` visited
            ]

        newMap = foldr (uncurry Map.insert) acc successors
        newVisited = foldr (Set.insert . fst) visited successors
        newQueue = successors

distinguish ::
    ( Automaton aut s i o
    , FiniteOrd i
    , Ord s
    , Eq o
    ) =>
    aut s i o ->
    s ->
    s ->
    [i]
distinguish m s1 s2 = explore Map.empty [(s1, s2, [])]
  where
    alphabet = Set.toList (inputs m)

    explore _ [] = []
    explore visited ((q1, q2, prefix) : queue)
        | Just symbol <- discrepancy = reverse (symbol : prefix)
        | otherwise = explore newVisited (queue ++ newQueue)
      where
        newVisited = Map.insert (q1, q2) prefix visited
        mo1 = update m q1
        mo2 = update m q2

        (nextStates1, outputs1) = unzip $ map (stepAndCurrent mo1) alphabet
        (nextStates2, outputs2) = unzip $ map (stepAndCurrent mo2) alphabet

        discrepancy = snd <$> List.find fst (zip (zipWith (/=) outputs1 outputs2) alphabet)

        appended = map (: prefix) alphabet

        toBeVisited = Map.fromList $ zip (zip nextStates1 nextStates2) appended

        newQueue = [(s1', s2', p) | ((s1', s2'), p) <- Map.toList toBeVisited, (s1', s2') `Map.notMember` visited]

    stepAndCurrent mo i = Bif.first current (step mo i)

{- | Returns a set of lists of inputs that can be used to distinguish between the given state and
 - any other state of the automaton.
-}
localCharacterizingSet ::
    ( Automaton aut s i o
    , FiniteOrd i
    , FiniteOrd s
    , Eq o
    ) =>
    aut s i o ->
    s ->
    Set.Set [i]
localCharacterizingSet m s = Set.fromList [d s sx | sx <- Set.toList $ states m, s /= sx]
  where
    d = distinguish m

{- | Returns a set of lists of inputs that can be used to distinguish between any two different states
of the automaton.
-}
globalCharacterizingSet ::
    ( Automaton aut s i o
    , FiniteOrd i
    , FiniteOrd s
    , Eq o
    ) =>
    aut s i o ->
    Set.Set [i]
globalCharacterizingSet m = Set.fromList [d s1 s2 | s1 <- sts, s2 <- sts, s1 < s2]
  where
    sts = Set.toList $ states m
    d = distinguish m
