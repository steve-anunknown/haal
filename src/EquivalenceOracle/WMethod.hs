{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements the W-method equivalence oracle.
module EquivalenceOracle.WMethod (
    WMethod (..),
    wmethodSuiteSize,
    RandomWMethod (..),
    RandomWMethodConfig (..),
) where

import BlackBox (Automaton, accessSequences, globalCharacterizingSet, inputs)
import Control.Monad (replicateM)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import EquivalenceOracle.RandomWords
import Experiment
import System.Random (Random (randomR, randomRs), RandomGen (split), StdGen)
import qualified Data.Vector as Vec

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

-- | The 'wmethodSuite' function generates the test suite for the W-method and a new oracle.
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
    (WMethod, [[i]])
wmethodSuite (WMethod{depth = d}) aut = (WMethod{depth = d}, suite)
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

-- | The 'RandomWMethodConfig' type is used to configure the random W-method.
data RandomWMethodConfig
    = RandomWMethodConfig
        -- | Random generator
        StdGen
        -- | Walks per state
        Int
        -- | Length of walk
        Int
    deriving (Show, Eq)

-- | The 'RandomWMethod' type represents a random W-method equivalence oracle.
newtype RandomWMethod = RandomWMethod RandomWMethodConfig deriving (Show, Eq)

-- | The 'randomWMethodSuite' function generates the test suite for the random W-method and a new oracle.
randomWMethodSuite ::
    forall i o s aut.
    ( Automaton aut s
    , Ord i
    , Ord s
    , Eq o
    , Bounded i
    , Bounded s
    , Enum i
    , Enum s
    ) =>
    RandomWMethod ->
    aut i o ->
    (RandomWMethod, [[i]])
randomWMethodSuite (RandomWMethod (RandomWMethodConfig g wpr wl)) aut = (RandomWMethod (RandomWMethodConfig gen''' wpr wl), suite)
  where
    shorthand = error "RandomWMethodSuite: This shouldn't be evaluated" :: aut i o
    rorc = RandomWords g wpr 0 wl
    prefixes = accessSequences aut
    globalCharSet = globalCharacterizingSet aut
    vecSuffixes = Vec.fromList $ Set.toList globalCharSet

    (RandomWords gen' _ _ _, randomwords) = List.mapAccumL testSuite rorc (replicate (length prefixes) shorthand)
    randomWords = concat randomwords

    (gen'', gen''') = split gen'

    -- make a random choice from globalCharSet (length prefix) times (wpr)
    samples = length prefixes * wpr
    randomSuffixes = take samples $ randomRs (0, Vec.length vecSuffixes - 1) gen''
    suffixes = map (vecSuffixes Vec.!) randomSuffixes

    suite = [prefix ++ rand ++ suffix | prefix <- Map.elems prefixes, rand <- randomWords, suffix <- suffixes]

instance EquivalenceOracle RandomWMethod where
    testSuite = randomWMethodSuite
