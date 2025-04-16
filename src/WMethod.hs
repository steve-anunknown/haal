module WMethod (
    WMethod (..),
    wmethod,
    wmethodSuite,
    wmethodSuiteSize,
) where

import BlackBox (Automaton, SUL, StateID, accessSequences, globalCharacterizingSet, inputs, reset, step, walk)
import Control.Monad (replicateM)
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set
import Experiment (EquivalenceOracle (..), Experiment, experiment)
import Lstar
import MealyAutomaton

newtype WMethod = WMethod {depth :: Int} deriving (Show, Eq)

wmethodSuiteSize ::
    ( Automaton aut s
    , Ord i
    , Ord s
    , Eq o
    , Bounded i
    , Bounded s
    , Enum i
    , Enum s
    ) =>
    WMethod ->
    aut i o ->
    Int
wmethodSuiteSize (WMethod{depth = d}) aut = size
  where
    alphabet = length (Set.toList $ inputs aut)
    accessSeqs = length (Map.elems $ accessSequences aut)
    characterizingSet = length (Set.toList $ globalCharacterizingSet aut)
    transitionCover = accessSeqs * alphabet
    size = sum [transitionCover * (alphabet ^ n) * characterizingSet | n <- [0 .. d]]

wmethodSuite ::
    ( Automaton aut s
    , Ord i
    , Ord s
    , Eq o
    , Bounded i
    , Bounded s
    , Enum i
    , Enum s
    ) =>
    WMethod ->
    aut i o ->
    [[i]]
wmethodSuite (WMethod{depth = d}) aut = suite
  where
    alphabet = Set.toList $ inputs aut
    accessSeqs = accessSequences aut
    characterizingSet = Set.toList $ globalCharacterizingSet aut
    transitionCover = [a ++ [inp] | a <- Map.elems accessSeqs, inp <- alphabet]
    suite =
        concat
            [ [acc ++ middle ++ char | acc <- transitionCover, char <- characterizingSet]
            | fixed <- [0 .. d]
            , middle <- replicateM fixed alphabet
            ]

wmethod ::
    ( Automaton aut s
    , Ord i
    , Ord s
    , Eq o
    , Bounded s
    , SUL sul
    , Bounded i
    , Enum i
    , Enum s
    ) =>
    WMethod ->
    aut i o ->
    Experiment (sul i o) (Maybe ([i], [o]))
wmethod (WMethod{depth = d}) aut = do
    sul <- ask
    let
        suite = wmethodSuite (WMethod d) aut

        pairwiseWalk _ _ [] = True
        pairwiseWalk theSul theAut (s : ss) = (out1 == out2) && pairwiseWalk sul' aut' ss
          where
            (sul', out1) = step theSul s
            (aut', out2) = step theAut s

        execute [] = Nothing
        execute (s : ss) = if continue then execute ss else Just (s, snd $ walk (reset sul) s)
          where
            continue = pairwiseWalk (reset sul) (reset aut) s
    return $ execute suite

instance EquivalenceOracle WMethod where
    testSuiteSize = wmethodSuiteSize
    testSuite = wmethodSuite
    findCex = wmethod

data Input = A | B deriving (Show, Eq, Ord, Bounded, Enum)
data Output = X | Y deriving (Show, Eq, Ord, Bounded, Enum)
data State = S0 | S1 | S2 deriving (Show, Eq, Ord, Bounded, Enum)

sulTransitions :: State -> Input -> (State, Output)
sulTransitions S0 _ = (S1, X)
sulTransitions S1 _ = (S2, Y)
sulTransitions S2 A = (S0, X)
sulTransitions S2 B = (S0, Y)

mysul = mkMealyAutomaton2 sulTransitions S0

myexperiment :: (SUL sul) => Experiment (sul Input Output) (MealyAutomaton StateID Input Output)
myexperiment = do
    let thelearner = Lstar (undefined :: ObservationTable Input Output)
    experiment thelearner (WMethod 2)

learnedmodel = runReader myexperiment mysul
