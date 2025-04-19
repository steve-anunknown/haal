-- | This module implements the W-method equivalence oracle.
module EquivalenceOracle.WMethod (
    WMethod (..),
    wmethodSuiteSize,
) where

import BlackBox (Automaton, accessSequences, globalCharacterizingSet, inputs)
import Control.Monad (replicateM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Experiment (EquivalenceOracle (..))

{- | The 'WMethod' type represents the W-method equivalence oracle.
It is just a wrapper around an integer, which is used for configuring
the exploration depth of the method.
-}
newtype WMethod = WMethod {depth :: Int} deriving (Show, Eq)

-- | The 'wmethodSuiteSize' function computes the size of the test suite for the W-method.
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

-- | The 'wmethodSuite' function generates the test suite for the W-method.
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

instance EquivalenceOracle WMethod where
    testSuite = wmethodSuite
