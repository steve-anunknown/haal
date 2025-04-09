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
import Data.Data (Data (dataTypeOf), dataTypeConstrs, fromConstr)
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
data MealyAutomaton input output state = MealyAutomaton
    { mealyDelta :: state -> input -> state
    , mealyLambda :: state -> input -> output
    , mealyInitialS :: state
    , mealyCurrentS :: state
    }

{- | The 'mkMealyAutomaton' constructor returns a 'MealyAutomaton' by requiring the 'mealyDelta'
function, the 'mealyLambda' function and the initial state 'mealyInitialS'.
-}
mkMealyAutomaton :: (s -> i -> s) -> (s -> i -> o) -> s -> MealyAutomaton i o s
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
mkMealyAutomaton2 :: (s -> i -> (s, o)) -> s -> MealyAutomaton i o s
mkMealyAutomaton2 transitions initial =
    MealyAutomaton
        { mealyDelta = \s i -> fst (transitions s i)
        , mealyLambda = \s i -> snd (transitions s i)
        , mealyInitialS = initial
        , mealyCurrentS = initial
        }

-- | Convenience function that updates the current state 'mealyCurrentS' of the automaton.
mealyUpdateState :: MealyAutomaton i o s -> s -> MealyAutomaton i o s
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
mealyStep :: MealyAutomaton i o s -> i -> (MealyAutomaton i o s, o)
mealyStep m i = (mealyUpdateState m nextState, output)
  where
    nextState = mealyDelta m (mealyCurrentS m) i
    output = mealyLambda m (mealyCurrentS m) i

{- | Extends the 'mealyStep' function by taking a series of inputs and performing multiple steps in
the automaton, returning a tuple containing the automaton with a modified state as well as the
outputs produced by the transitions.
-}
mealyWalk :: (Traversable t) => MealyAutomaton i o s -> t i -> (MealyAutomaton i o s, t o)
mealyWalk = List.mapAccumL mealyStep

-- | Resets the automaton to its initial state.
mealyReset :: MealyAutomaton i o s -> MealyAutomaton i o s
mealyReset m = mealyUpdateState m (mealyInitialS m)

{- | Returns a map describing the combined behaviour of the 'mealyDelta'
and 'mealyLambda' functions.
-}
mealyTransitions ::
    forall i o s.
    (Data s, Data i, Ord s, Ord i) =>
    MealyAutomaton i o s ->
    Map.Map (s, i) (s, o)
mealyTransitions m = Map.fromList [((s, i), (delta s i, lambda s i)) | s <- domainS, i <- domainI]
  where
    delta = mealyDelta m
    lambda = mealyLambda m
    constructorsS = dataTypeConstrs $ dataTypeOf (undefined :: s)
    constructorsI = dataTypeConstrs $ dataTypeOf (undefined :: i)
    domainS = List.map fromConstr constructorsS
    domainI = List.map fromConstr constructorsI

-- | Returns a set of all values of the input alphabet of the automaton.
mealyInAlphabet ::
    forall i o s.
    (Ord i, Data i) =>
    MealyAutomaton i o s ->
    Set.Set i
mealyInAlphabet _ = Set.fromList (List.map fromConstr (dataTypeConstrs $ dataTypeOf (undefined :: i)) :: [i])

-- | Returns a set of all values of the output alphabet of the automaton.
mealyOutAlphabet ::
    forall i o s.
    (Ord o, Data o) =>
    MealyAutomaton i o s ->
    Set.Set o
mealyOutAlphabet _ = Set.fromList (List.map fromConstr (dataTypeConstrs $ dataTypeOf (undefined :: o)) :: [o])

-- | Returns a set of all values of the states of the automaton.
mealyStates ::
    forall i o s.
    (Ord s, Data s) =>
    MealyAutomaton i o s ->
    Set.Set s
mealyStates _ = Set.fromList (List.map fromConstr (dataTypeConstrs $ dataTypeOf (undefined :: s)) :: [s])

{- | Returns a set of lists of inputs that can be used to distinguish between any two different states
of the automaton.
-}
mealyGlobalCharacterizingSet ::
    (Ord i, Data i, Ord s, Data s, Eq o) =>
    MealyAutomaton i o s ->
    Set.Set [i]
mealyGlobalCharacterizingSet m = Set.fromList [distinguish s1 s2 | s1 <- states, s2 <- states, s1 < s2]
  where
    states = Set.toList $ mealyStates m
    distinguish = mealyDistinguishingSequence m

{- | Returns a set of lists of inputs that can be used to distinguish between the given state and
 - any other state of the automaton.
-}
mealyLocalCharacterizingSet ::
    (Ord i, Data i, Ord s, Data s, Eq o) =>
    MealyAutomaton i o s ->
    s ->
    Set.Set [i]
mealyLocalCharacterizingSet m s = Set.fromList [distinguish s sx | sx <- states, s /= sx]
  where
    states = Set.toList $ mealyStates m
    distinguish = mealyDistinguishingSequence m

-- | Returns a list of inputs that can be used to distinguish between the two given states of the automaton.
mealyDistinguishingSequence ::
    forall i o s.
    (Ord i, Data i, Ord s, Eq o) =>
    MealyAutomaton i o s ->
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
    forall i o s.
    (Ord i, Data i, Ord s) =>
    MealyAutomaton i o s ->
    Map.Map s [i]
mealyAccessSequences m = explore [(initialState, [])] Set.empty (Map.singleton initialState [])
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
        newVisited = List.foldr Set.insert visited (q : List.map fst nextStates)
        newQueue = qs ++ nextStates

instance BlackBox.SUL MealyAutomaton where
    step = mealyStep
    walk = mealyWalk
    reset = mealyReset
    inputs = mealyInAlphabet
    outputs = mealyOutAlphabet

instance BlackBox.Automaton MealyAutomaton where
    current = mealyCurrentS
    transitions = mealyTransitions
    states = mealyStates
    localCharacterizingSet = mealyLocalCharacterizingSet
    globalCharacterizingSet = mealyGlobalCharacterizingSet

instance
    ( Show i
    , Show o
    , Show s
    , Data s
    , Data i
    , Ord s
    , Ord i
    ) =>
    Show (MealyAutomaton i o s)
    where
    show m =
        "{\n\tTransitions: "
            ++ show transitions
            ++ ",\n\tCurrent State: "
            ++ show current
            ++ ",\n\tInitial State: "
            ++ show initial
            ++ "\n}"
      where
        transitions = mealyTransitions m
        initial = mealyInitialS m
        current = mealyCurrentS m

instance
    ( Ord s
    , Ord i
    , Eq o
    , Data s
    , Data i
    ) =>
    Eq (MealyAutomaton i o s)
    where
    m1 == m2 =
        mealyTransitions m1 == mealyTransitions m2
            && mealyInitialS m1 == mealyInitialS m2
