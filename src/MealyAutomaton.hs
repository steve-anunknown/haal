{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements a Mealy automaton.
module MealyAutomaton (
    MealyAutomaton (..),
    mkMealyAutomaton,
    mkMealyAutomaton2,
    mealyStep,
    mealyWalk,
    mealyReset,
    mealyTransitions,
    mealyStates,
    mealyUpdateState,
    mealyInAlphabet,
    mealyOutAlphabet,
    mealyDistinguishingSequence,
    mealyGlobalCharacterizingSet,
    mealyLocalCharacterizingSet,
    mealyAccessSequences,
)
where

import qualified BlackBox
import qualified Data.Bifunctor as Bif
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

{- | The 'MealyAutomaton' data type is parameterised by the 'input', 'output' and 'state' types
 which play the role of the input alphabet, output alphabet and set of states respectively.
 The transitions of the automaton are defined by the 'mealyDelta' and 'mealyLambda' functions,
 which respectively return the new state after a transition and the produced output. Finally,
 the 'mealyInitialS' defines the initial state of the automaton and the 'mealyCurrentS' defines
 the current state which the automaton is in.
-}
data MealyAutomaton state input output = MealyAutomaton
    { mealyDelta :: state -> input -> state
    , mealyLambda :: state -> input -> output
    , mealyInitialS :: state
    , mealyCurrentS :: state
    }

{- | The 'mkMealyAutomaton' constructor returns a 'MealyAutomaton' by requiring the 'mealyDelta'
function, the 'mealyLambda' function and the initial state 'mealyInitialS'.
-}
mkMealyAutomaton :: (s -> i -> s) -> (s -> i -> o) -> s -> MealyAutomaton s i o
mkMealyAutomaton delta lambda initial =
    MealyAutomaton
        { mealyDelta = delta
        , mealyLambda = lambda
        , mealyInitialS = initial
        , mealyCurrentS = initial
        }

{- | The 'mkMealyAutomaton2' constructor returns a 'MealyAutomaton' by requiring just one
function describing both the state transitions as well as the produced outputs, instead of two
separate functions, and the initial state 'mealyInitialS'.
-}
mkMealyAutomaton2 :: (s -> i -> (s, o)) -> s -> MealyAutomaton s i o
mkMealyAutomaton2 transitions initial =
    MealyAutomaton
        { mealyDelta = \s i -> fst (transitions s i)
        , mealyLambda = \s i -> snd (transitions s i)
        , mealyInitialS = initial
        , mealyCurrentS = initial
        }

-- | Convenience function that updates the current state 'mealyCurrentS' of the automaton.
mealyUpdateState :: MealyAutomaton s i o -> s -> MealyAutomaton s i o
mealyUpdateState m s =
    MealyAutomaton
        { mealyDelta = delta
        , mealyLambda = lambda
        , mealyInitialS = initial
        , mealyCurrentS = s
        }
  where
    delta = mealyDelta m
    lambda = mealyLambda m
    initial = mealyInitialS m

{- | Performs a step in the automaton and returns a tuple containing the automaton with a modified
state as well as the output produced by the transition.
-}
mealyStep :: MealyAutomaton s i o -> i -> (MealyAutomaton s i o, o)
mealyStep m i = (mealyUpdateState m nextState, output)
  where
    nextState = mealyDelta m (mealyCurrentS m) i
    output = mealyLambda m (mealyCurrentS m) i

{- | Extends the 'mealyStep' function by taking a series of inputs and performing multiple steps in
the automaton, returning a tuple containing the automaton with a modified state as well as the
outputs produced by the transitions.
-}
mealyWalk :: MealyAutomaton s i o -> [i] -> (MealyAutomaton s i o, [o])
mealyWalk = List.mapAccumL mealyStep

-- | Resets the automaton to its initial state.
mealyReset :: MealyAutomaton s i o -> MealyAutomaton s i o
mealyReset m = mealyUpdateState m (mealyInitialS m)

{- | Returns a map describing the combined behaviour of the 'mealyDelta'
and 'mealyLambda' functions.
-}
mealyTransitions ::
    forall s i o.
    (Bounded s, Bounded i, Ord s, Ord i, Enum s, Enum i) =>
    MealyAutomaton s i o ->
    Map.Map (s, i) (s, o)
mealyTransitions m = error "TODO: handle infinite states"
  where
    delta = mealyDelta m
    lambda = mealyLambda m
    domainS = [minBound .. maxBound] :: [s]
    domainI = Set.toList $ mealyInAlphabet m

-- | Returns a set of all values of the input alphabet of the automaton.
mealyInAlphabet ::
    forall s i o.
    (Ord i, Bounded i, Enum i) =>
    MealyAutomaton s i o ->
    Set.Set i
mealyInAlphabet _ = Set.fromList [minBound .. maxBound] :: Set.Set i

-- | Returns a set of all values of the output alphabet of the automaton.
mealyOutAlphabet ::
    forall s i o.
    (Ord o, Bounded o, Enum o) =>
    MealyAutomaton s i o ->
    Set.Set o
mealyOutAlphabet _ = Set.fromList [minBound .. maxBound] :: Set.Set o

-- | Returns a set of all values of the states of the automaton.
mealyStates ::
    forall s i o.
    (Ord s, Bounded s, Enum s) =>
    MealyAutomaton s i o ->
    Set.Set s
mealyStates _ = Set.fromList [minBound .. maxBound] :: Set.Set s

{- | Returns a set of lists of inputs that can be used to distinguish between any two different states
of the automaton.
-}
mealyGlobalCharacterizingSet ::
    (Ord i, Ord s, Eq o, Bounded s, Enum s, Bounded i, Enum i) =>
    MealyAutomaton s i o ->
    Set.Set [i]
mealyGlobalCharacterizingSet m = Set.fromList [distinguish s1 s2 | s1 <- states, s2 <- states, s1 < s2]
  where
    states = Set.toList $ mealyStates m
    distinguish = mealyDistinguishingSequence m

{- | Returns a set of lists of inputs that can be used to distinguish between the given state and
 - any other state of the automaton.
-}
mealyLocalCharacterizingSet ::
    (Ord i, Ord s, Eq o, Bounded s, Enum s, Bounded i, Enum i) =>
    MealyAutomaton s i o ->
    s ->
    Set.Set [i]
mealyLocalCharacterizingSet m s = Set.fromList [distinguish s sx | sx <- states, s /= sx]
  where
    states = Set.toList $ mealyStates m
    distinguish = mealyDistinguishingSequence m

-- | Returns a list of inputs that can be used to distinguish between the two given states of the automaton.
mealyDistinguishingSequence ::
    forall s i o.
    (Ord i, Ord s, Eq o, Bounded i, Enum i) =>
    MealyAutomaton s i o ->
    s ->
    s ->
    [i]
mealyDistinguishingSequence _ s1 s2 | s1 == s2 = []
mealyDistinguishingSequence m s1 s2 = explore Map.empty [(s1, s2, [])]
  where
    alphabet = Set.toList (mealyInAlphabet m)

    explore _ [] = []
    explore visited ((q1, q2, prefix) : queue)
        | Just seqFound <- discrepancy = reverse (seqFound : prefix)
        | otherwise = explore newVisited (queue ++ newQueue)
      where
        newVisited = Map.insert (q1, q2) prefix visited

        (nextStates1, outputs1) = List.unzip $ List.map (mealyStepPair m q1) alphabet
        (nextStates2, outputs2) = List.unzip $ List.map (mealyStepPair m q2) alphabet

        discrepancy = List.elemIndex False (List.zipWith (==) outputs1 outputs2) >>= \idx -> Just (alphabet !! idx)

        toBeVisited = Map.fromList [((s1', s2'), i : prefix) | (s1', s2', i) <- zip3 nextStates1 nextStates2 alphabet]
        newQueue = [(s1', s2', p) | ((s1', s2'), p) <- Map.toList toBeVisited, (s1', s2') `Map.notMember` visited]

    mealyStepPair mo state i = Bif.first mealyCurrentS (mealyStep (mealyUpdateState mo state) i)

-- | Returns a map containing the shortest sequence to access a given state from the initial state.
mealyAccessSequences ::
    forall s i o.
    (Ord i, Ord s, Bounded i, Enum i) =>
    MealyAutomaton s i o ->
    Map.Map s [i]
mealyAccessSequences m = explore [(initialState, [])] (Set.singleton initialState) (Map.singleton initialState [])
  where
    alphabet = Set.toList (mealyInAlphabet m)
    initialState = mealyInitialS m

    explore :: [(s, [i])] -> Set.Set s -> Map.Map s [i] -> Map.Map s [i]
    explore [] _ theMap = Map.map List.reverse theMap
    explore ((q, pre) : qs) visited theMap = explore newQueue newVisited newMap
      where
        mo = mealyUpdateState m q
        nextStates =
            List.map
                (Bif.second (: pre))
                ( List.filter ((`Set.notMember` visited) . fst) $
                    List.zip (List.map (mealyCurrentS . fst . mealyStep mo) alphabet) alphabet
                )
        newMap = List.foldr (uncurry Map.insert) theMap nextStates
        newVisited = foldr (Set.insert . fst) visited nextStates
        newQueue = qs ++ nextStates

instance BlackBox.SUL (MealyAutomaton s) where
    step = mealyStep
    walk = mealyWalk
    reset = mealyReset
    inputs = mealyInAlphabet
    outputs = mealyOutAlphabet

instance BlackBox.Automaton (MealyAutomaton s) s where
    current = mealyCurrentS
    transitions = mealyTransitions
    states = mealyStates
    accessSequences = mealyAccessSequences
    localCharacterizingSet = mealyLocalCharacterizingSet
    globalCharacterizingSet = mealyGlobalCharacterizingSet

instance
    ( Show i
    , Show o
    , Show s
    , Bounded s
    , Bounded i
    , Enum s
    , Enum i
    , Ord s
    , Ord i
    ) =>
    Show (MealyAutomaton s i o)
    where
    show m =
        "{\n\tCurrent State: "
            ++ show current
            ++ ",\n\tInitial State: "
            ++ show initial
            ++ ",\n\tTransitions: "
            ++ show transitions
            ++ "\n}"
      where
        transitions = mealyTransitions m
        initial = mealyInitialS m
        current = mealyCurrentS m

instance
    ( Ord s
    , Ord i
    , Eq o
    , Bounded s
    , Bounded i
    , Enum s
    , Enum i
    ) =>
    Eq (MealyAutomaton s i o)
    where
    m1 == m2 =
        mealyTransitions m1 == mealyTransitions m2
            && mealyInitialS m1 == mealyInitialS m2
