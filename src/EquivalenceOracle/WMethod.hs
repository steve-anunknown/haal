{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements the W-method equivalence oracle.
module EquivalenceOracle.WMethod (
    WMethod (..),
    WMethodConfig (..),
    wmethodSuiteSize,
    RandomWMethod (..),
    RandomWMethodConfig (..),
) where

import BlackBox (Automaton, accessSequences, globalCharacterizingSet, inputs)
import Control.Monad (replicateM)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import EquivalenceOracle.RandomWords
import Experiment
import System.Random (Random (randomRs), RandomGen (split), StdGen)

newtype WMethodConfig = WMethodConfig
    { wmDepth :: Int
    -- ^ The maximum depth of the W-method.
    }
    deriving (Show, Eq)

{- | The 'WMethod' type represents the W-method equivalence oracle.
It is just a wrapper around an integer, which is used for configuring
the exploration depth of the method.
-}
newtype WMethod = WMethod WMethodConfig deriving (Show, Eq)

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
wmethodSuiteSize (WMethod (WMethodConfig{wmDepth = d})) aut = size
  where
    alphabet = length $ inputs aut
    accessSeqs = length $ accessSequences aut
    characterizingSet = length $ globalCharacterizingSet aut
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
wmethodSuite wm@(WMethod (WMethodConfig{wmDepth = d})) aut = (wm, suite)
  where
    alphabet = Set.toList $ inputs aut
    accessSeqs = accessSequences aut
    characterizingSet = Set.toList $ globalCharacterizingSet aut
    transitionCover = [a ++ [inp] | a <- Map.elems accessSeqs, inp <- alphabet]
    middlesByDepth = [replicateM n alphabet | n <- [0 .. d]]
    suite =
        concat
            [ [acc ++ middle ++ char | acc <- transitionCover, char <- characterizingSet]
            | middles <- middlesByDepth
            , middle <- middles
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
randomWMethodSuite (RandomWMethod (RandomWMethodConfig g wpr wl)) aut =
    let rorc = RandomWords (RandomWordsConfig{rwMaxLength = wl, rwMinLength = 1, rwLimit = wpr, rwGen = g})
        prefixes = Map.elems $ accessSequences aut
        vecSuffixes = Vec.fromList $ Set.toList $ globalCharacterizingSet aut

        (RandomWords roc', wordBatches) = List.mapAccumL testSuite rorc (replicate (length prefixes) (undefined :: aut i o))
        flatWords = concat wordBatches

        (gen'', gen''') = split (rwGen roc')
        samples = length prefixes * wpr
        randomSuffixes = take samples $ randomRs (0, Vec.length vecSuffixes - 1) gen''
        suffixes = map (vecSuffixes Vec.!) randomSuffixes

        suite = [prefix ++ rand ++ suffix | prefix <- prefixes, rand <- flatWords, suffix <- suffixes]
     in (RandomWMethod (RandomWMethodConfig gen''' wpr wl), suite)

instance EquivalenceOracle RandomWMethod where
    testSuite = randomWMethodSuite
