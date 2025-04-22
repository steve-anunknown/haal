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
    transitions :: (Bounded st, Bounded i, Enum st, Enum i, Ord st, Ord i) => aut i o -> Map.Map (st, i) (st, o)
    states :: (Ord st, Bounded st, Enum st) => aut i o -> Set.Set st
    current :: aut i o -> st
    update :: aut i o -> st -> aut i o

initial :: (Automaton aut st) => aut i o -> st
initial = current . reset

-- | Returns a map containing the shortest sequence to access a given state from the initial state.
accessSequences ::
    forall s i o aut.
    (Ord i, Bounded i, Enum i, Ord s, Automaton aut s) =>
    aut i o ->
    Map.Map s [i]
accessSequences m = explore [(initialState, [])] (Set.singleton initialState) (Map.singleton initialState [])
  where
    alphabet = Set.toList (inputs m)
    initialState = initial m

    explore :: [(s, [i])] -> Set.Set s -> Map.Map s [i] -> Map.Map s [i]
    explore [] _ theMap = Map.map List.reverse theMap
    explore ((q, pre) : qs) visited theMap = if q == current mo then explore newQueue newVisited newMap else error "what the fuck"
      where
        mo = fst $ walk (reset m) (List.reverse pre)
        nextStates =
            List.map
                (Bif.second (: pre))
                ( List.filter ((`Set.notMember` visited) . fst) $
                    List.zip (List.map (current . fst . step mo) alphabet) alphabet
                )
        newMap = List.foldr (uncurry Map.insert) theMap nextStates
        newVisited = List.foldr (Set.insert . fst) visited nextStates
        newQueue = qs ++ nextStates

distinguish :: (Bounded i, Enum i, Ord i, Eq o, Ord s, Automaton aut s) => aut i o -> s -> s -> [i]
distinguish m s1 s2 = explore Map.empty [(s1, s2, [])]
  where
    alphabet = Set.toList (inputs m)

    explore _ [] = []
    explore visited ((q1, q2, prefix) : queue)
        | Just seqFound <- discrepancy = reverse (seqFound : prefix)
        | otherwise = explore newVisited (queue ++ newQueue)
      where
        newVisited = Map.insert (q1, q2) prefix visited

        (nextStates1, outputs1) = List.unzip $ List.map (updateAndStep m q1) alphabet
        (nextStates2, outputs2) = List.unzip $ List.map (updateAndStep m q2) alphabet

        discrepancy = List.elemIndex False (List.zipWith (==) outputs1 outputs2) >>= \idx -> Just (alphabet !! idx)

        toBeVisited = Map.fromList [((s1', s2'), i : prefix) | (s1', s2', i) <- zip3 nextStates1 nextStates2 alphabet]
        newQueue = [(s1', s2', p) | ((s1', s2'), p) <- Map.toList toBeVisited, (s1', s2') `Map.notMember` visited]

    updateAndStep mo s i = Bif.first current (step (update mo s) i)

{- | Returns a set of lists of inputs that can be used to distinguish between the given state and
 - any other state of the automaton.
-}
localCharacterizingSet ::
    (Ord i, Ord s, Eq o, Bounded i, Enum i, Automaton aut s, Bounded s, Enum s) =>
    aut i o ->
    s ->
    Set.Set [i]
localCharacterizingSet m s = Set.fromList [d s sx | sx <- Set.toList $ states m, s /= sx]
  where
    d = distinguish m

{- | Returns a set of lists of inputs that can be used to distinguish between any two different states
of the automaton.
-}
globalCharacterizingSet ::
    (Ord i, Ord s, Eq o, Bounded i, Enum i, Automaton aut s, Bounded s, Enum s) =>
    aut i o ->
    Set.Set [i]
globalCharacterizingSet m = Set.fromList [d s1 s2 | s1 <- sts, s2 <- sts, s1 < s2]
  where
    sts = Set.toList $ states m
    d = distinguish m
