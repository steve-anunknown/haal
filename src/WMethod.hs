module WMethod (
    WMethod (..),
    wmethod,
    wmethodSuite,
    wmethodSuiteSize,
) where

import BlackBox (Automaton, SUL, accessSequences, globalCharacterizingSet, inputs, reset, step, walk)
import Control.Monad (replicateM)
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set
import EquivalenceOracle (EquivalenceOracle (..))
import Experiment (Experiment)

newtype WMethod = WMethod {depth :: Int} deriving (Show, Eq)

wmethodSuiteSize ::
    ( Automaton aut
    , Ord i
    , Ord s
    , Eq o
    , Bounded i
    , Bounded s
    , Enum i
    , Enum s
    ) =>
    WMethod ->
    aut i o s ->
    Int
wmethodSuiteSize (WMethod{depth = d}) aut = size
  where
    alphabet = length (Set.toList $ inputs aut)
    accessSeqs = length (Map.elems $ accessSequences aut)
    characterizingSet = length (Set.toList $ globalCharacterizingSet aut)
    transitionCover = accessSeqs * alphabet
    size = sum [transitionCover * (alphabet ^ n) * characterizingSet | n <- [0 .. d]]

wmethodSuite ::
    ( Automaton aut
    , Ord i
    , Ord s
    , Eq o
    , Bounded i
    , Bounded s
    , Enum i
    , Enum s
    ) =>
    WMethod ->
    aut i o s ->
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
    ( Automaton aut
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
    aut i o s ->
    Experiment (sul i o s) (Maybe ([i], [o]))
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
