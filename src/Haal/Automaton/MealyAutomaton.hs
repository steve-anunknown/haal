{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements a Mealy automaton.
module Haal.Automaton.MealyAutomaton (
    MealyAutomaton,
    mkMealyAutomaton,
    mkMealyAutomaton2,
    mkMealyAutomatonTable,
    mealyDelta,
    mealyLambda,
    mealyTransitions,
)
where

import Data.Char (ord)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as VU
import Haal.BlackBox

{- | The 'MealyAutomaton' data type is parameterised by the @input@, @output@ and @state@ types
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
    , mealyStates :: Set.Set state
    }

{- | The 'mkMealyAutomaton' constructor returns a 'MealyAutomaton' by requiring the 'mealyDelta'
function, the 'mealyLambda' function and the initial state 'mealyInitialS'.
-}
mkMealyAutomaton :: (s -> i -> s) -> (s -> i -> o) -> Set.Set s -> s -> MealyAutomaton s i o
mkMealyAutomaton delta lambda sts initS =
    MealyAutomaton
        { mealyDelta = delta
        , mealyLambda = lambda
        , mealyInitialS = initS
        , mealyCurrentS = initS
        , mealyStates = sts
        }

{- | The 'mkMealyAutomaton2' constructor returns a 'MealyAutomaton' by requiring just one
function describing both the state transitions as well as the produced outputs, instead of two
separate functions, and the initial state 'mealyInitialS'.
-}
mkMealyAutomaton2 :: (s -> i -> (s, o)) -> Set.Set s -> s -> MealyAutomaton s i o
mkMealyAutomaton2 transs sts initS =
    MealyAutomaton
        { mealyDelta = \s i -> fst (transs s i)
        , mealyLambda = \s i -> snd (transs s i)
        , mealyInitialS = initS
        , mealyCurrentS = initS
        , mealyStates = sts
        }

{- | The 'mkMealyAutomatonTable' constructor returns a 'MealyAutomaton' with states
@0 .. n - 1@ from a transition table and an output table, each encoded as a 'String'
in which every 'Char' stands for the number @'ord' c@.

  @mkMealyAutomatonTable n initS deltaTable lambdaTable@ expects both tables to hold
  one entry per state and input, in row-major order: the entry at position
  @s * k + j@, where @k@ is the number of inputs and @j@ is the position of the input
  in @[minBound .. maxBound]@, describes state @s@ on that input. An entry of
  @deltaTable@ is the next state, and an entry of @lambdaTable@ is the position of
  the output in @[minBound .. maxBound]@.

  String literals compile far faster than large pattern matches, which is why
  @haal-gen@ emits its models in this form.

  Returns @'Left' err@ if the number of states is not positive, the initial state is
  out of range, a table has the wrong length, or an entry is out of range. Applying
  the resulting transition functions to a state outside @0 .. n - 1@ is an error.
-}
{-# INLINEABLE mkMealyAutomatonTable #-}
mkMealyAutomatonTable ::
    forall i o.
    (Finite i, Finite o) =>
    Int ->
    Int ->
    String ->
    String ->
    Either String (MealyAutomaton Int i o)
mkMealyAutomatonTable n initS deltaTable lambdaTable = do
    -- Only this wrapper is specialised at each use site (it is INLINABLE), so
    -- that lookups call 'fromEnum' and 'toEnum' directly rather than through
    -- a dictionary. Everything else happens in the monomorphic
    -- 'decodeTables', which keeps the specialised code, and therefore the
    -- compile time of each generated model, small.
    (sts, deltaV, lambdaV) <- decodeTables n numI numO initS deltaTable lambdaTable
    let index s i = s * numI + (fromEnum i - firstI)
        delta s i = deltaV VU.! index s i
        lambda s i = toEnum (firstO + lambdaV VU.! index s i)
    return (mkMealyAutomaton delta lambda sts initS)
  where
    firstI = fromEnum (minBound :: i)
    numI = fromEnum (maxBound :: i) - firstI + 1
    firstO = fromEnum (minBound :: o)
    numO = fromEnum (maxBound :: o) - firstO + 1

{- | Validate and decode the tables of 'mkMealyAutomatonTable', given the
number of states, inputs, and outputs and the initial state.
-}
{-# NOINLINE decodeTables #-}
decodeTables ::
    Int ->
    Int ->
    Int ->
    Int ->
    String ->
    String ->
    Either String (Set.Set Int, VU.Vector Int, VU.Vector Int)
decodeTables n numI numO initS deltaTable lambdaTable
    | n <= 0 = Left "the automaton must have at least one state"
    | initS < 0 || initS >= n =
        Left ("initial state " ++ show initS ++ " is not in 0 .. " ++ show (n - 1))
    | VU.length deltaV /= size =
        Left ("transition table has " ++ show (VU.length deltaV) ++ " entries, expected " ++ show size)
    | VU.length lambdaV /= size =
        Left ("output table has " ++ show (VU.length lambdaV) ++ " entries, expected " ++ show size)
    | VU.any (>= n) deltaV =
        Left "transition table refers to a state that does not exist"
    | VU.any (>= numO) lambdaV =
        Left "output table refers to an output that does not exist"
    | otherwise = Right (Set.fromDistinctAscList [0 .. n - 1], deltaV, lambdaV)
  where
    size = n * numI
    deltaV = VU.fromList (map ord deltaTable)
    lambdaV = VU.fromList (map ord lambdaTable)

{- | Performs a step in the automaton and returns a tuple containing the automaton with a modified
state as well as the output produced by the transition.
-}
mealyStep :: MealyAutomaton s i o -> i -> (MealyAutomaton s i o, o)
mealyStep m i = (m{mealyCurrentS = nextState}, output)
  where
    nextState = mealyDelta m (mealyCurrentS m) i
    output = mealyLambda m (mealyCurrentS m) i

-- | Resets the automaton to its initial state.
mealyReset :: MealyAutomaton s i o -> MealyAutomaton s i o
mealyReset m = m{mealyCurrentS = mealyInitialS m}

{- | An automaton is a SUL in any monad. Stepping it is pure, so it never uses
the monad; this lets the automaton be learned inside whatever monad the
experiment runs in, e.g. a 'Control.Monad.State.StateT' holding user-defined
statistics.
-}
instance (Monad m) => SUL (MealyAutomaton s) m where
    step sul i = return (mealyStep sul i)
    reset = return . mealyReset

{- | Returns a map describing the combined behaviour of the 'mealyDelta'
and 'mealyLambda' functions.
-}
mealyTransitions ::
    forall s i o.
    (Ord s, FiniteOrd i) =>
    MealyAutomaton s i o ->
    Map.Map (s, i) (s, o)
mealyTransitions m = Map.fromList [((s, i), (delta s i, lambda s i)) | s <- domainS, i <- domainI]
  where
    delta = mealyDelta m
    lambda = mealyLambda m
    domainS = Set.toList $ mealyStates m
    domainI = Set.toList $ inputs m

instance Automaton MealyAutomaton s where
    transitions = mealyTransitions
    states = mealyStates
    current = mealyCurrentS
    update m s = m{mealyCurrentS = s}

instance
    ( Show i
    , Show o
    , Show s
    , FiniteOrd s
    , FiniteOrd i
    ) =>
    Show (MealyAutomaton s i o)
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
        transs = mealyTransitions m
        initialS = initial m
        currentS = current m

instance
    ( FiniteOrd s
    , FiniteOrd i
    , Eq o
    ) =>
    Eq (MealyAutomaton s i o)
    where
    m1 == m2 =
        mealyTransitions m1 == mealyTransitions m2
            && mealyInitialS m1 == mealyInitialS m2
