{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | This module defines the BlackBox type class as well as the Automaton and SUL
sub classes.
-}
module BlackBox (
    Automaton (..),
    SUL (..),
    StateID,
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
import qualified Data.Set as Set

import qualified Data.HashMap.Strict as HMS
import Data.Hashable (Hashable)

{- | The 'StateID' type is an alias for an integer that represents the state of the automaton.
 - It is used as a default type for the state of learned automata.
-}
type StateID = Int

{- | The 'SUL' type class defines the basic interface for a black box automaton.
It provides methods to step through the automaton, walk through a list of inputs,
and retrieve the current state.
-}
class SUL sul where
    step :: (Ord i) => sul i o -> i -> (sul i o, o)
    reset :: sul i o -> sul i o

walk :: (Ord i, SUL sul) => sul i o -> [i] -> (sul i o, [o])
walk = List.mapAccumL step

inputs :: (Ord i, Bounded i, Enum i) => sul i o -> Set.Set i
inputs _ = Set.fromList [minBound .. maxBound]

outputs :: (Ord o, Bounded o, Enum o) => sul i o -> Set.Set o
outputs _ = Set.fromList [minBound .. maxBound]

{- | The 'Automaton' type class extends the 'SUL' type class and adds
support for automata operations.
-}
class (SUL aut) => Automaton aut st | aut -> st where
    transitions :: (Hashable st, Hashable i, Bounded st, Bounded i, Enum st, Enum i, Ord st, Ord i) => aut i o -> HMS.HashMap (st, i) (st, o)
    states :: (Ord st, Bounded st, Enum st) => aut i o -> Set.Set st
    current :: aut i o -> st
    update :: aut i o -> st -> aut i o

initial :: (Automaton aut st) => aut i o -> st
initial = current . reset

-- | Returns a map containing the shortest sequence to access each reachable state from the initial state.
accessSequences ::
    forall s i o aut.
    (Ord i, Bounded i, Enum i, Ord s, Automaton aut s, Hashable s) =>
    aut i o ->
    HMS.HashMap s [i]
accessSequences aut = bfs [(initialSt, [])] (Set.singleton initialSt) (HMS.singleton initialSt [])
  where
    alphabet = Set.toList (inputs aut)
    initialSt = initial aut

    bfs :: [(s, [i])] -> Set.Set s -> HMS.HashMap s [i] -> HMS.HashMap s [i]
    bfs [] _ acc = HMS.map List.reverse acc
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
        -- newMap = foldr (uncurry HMS.insert) acc successors
        -- newVisited = foldr (Set.insert . fst) visited successors

        (newVisited, newMap) =
            foldr
                ( \(state, pref) (vs, mp) ->
                    (Set.insert state vs, HMS.insert state pref mp)
                )
                (visited, acc)
                successors

        newQueue = successors

distinguish :: (Bounded i, Enum i, Ord i, Eq o, Ord s, Automaton aut s, Hashable s) => aut i o -> s -> s -> [i]
distinguish m s1 s2 = explore HMS.empty [(s1, s2, [])]
  where
    alphabet = Set.toList (inputs m)

    explore _ [] = []
    explore visited ((q1, q2, prefix) : queue)
        | Just seqFound <- discrepancy = reverse (seqFound : prefix)
        | otherwise = explore newVisited (queue ++ newQueue)
      where
        newVisited = HMS.insert (q1, q2) prefix visited
        mo1 = update m q1
        mo2 = update m q2

        (nextStates1, outputs1) = unzip $ map (stepAndCurrent mo1) alphabet
        (nextStates2, outputs2) = unzip $ map (stepAndCurrent mo2) alphabet

        discrepancy = List.elemIndex False (zipWith (==) outputs1 outputs2) >>= \idx -> Just (alphabet !! idx)

        appended = map (: prefix) alphabet

        toBeVisited = HMS.fromList $ zip (zip nextStates1 nextStates2) appended

        newQueue = [(s1', s2', p) | ((s1', s2'), p) <- HMS.toList toBeVisited, (s1', s2') `HMS.member` visited]

    stepAndCurrent mo i = Bif.first current (step mo i)

{- | Returns a set of lists of inputs that can be used to distinguish between the given state and
 - any other state of the automaton.
-}
localCharacterizingSet ::
    (Ord i, Ord s, Eq o, Bounded i, Enum i, Automaton aut s, Bounded s, Enum s, Hashable s) =>
    aut i o ->
    s ->
    Set.Set [i]
localCharacterizingSet m s = Set.fromList [d sx | sx <- Set.toList $ states m, s /= sx]
  where
    d = distinguish m s

{- | Returns a set of lists of inputs that can be used to distinguish between any two different states
of the automaton.
-}
globalCharacterizingSet ::
    (Ord i, Ord s, Eq o, Bounded i, Enum i, Automaton aut s, Bounded s, Enum s, Hashable s) =>
    aut i o ->
    Set.Set [i]
globalCharacterizingSet m = Set.fromList [d s1 s2 | s1 <- sts, s2 <- sts, s1 < s2]
  where
    sts = Set.toList $ states m
    d = distinguish m
