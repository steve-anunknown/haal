{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements a Mealy automaton.
module Haal.Automaton.MealyAutomaton (
    MealyAutomaton (..),
    mkMealyAutomaton,
    mkMealyAutomaton2,
    mealyStep,
    mealyReset,
    mealyTransitions,
)
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Haal.BlackBox
import Control.Monad.Identity (Identity)

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

instance SUL (MealyAutomaton s) Identity i o where
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

instance Automaton MealyAutomaton s i o where
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
