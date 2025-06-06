{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements the W-method equivalence oracle.
module Haal.EquivalenceOracle.WMethod (
    WMethod (..),
    WMethodConfig (..),
    wmethodSuiteSize,
    RandomWMethod (..),
    RandomWMethodConfig (..),
    mkWMethod,
    mkRandomWMethod,
) where

import Control.Monad (replicateM)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Haal.BlackBox
import Haal.EquivalenceOracle.RandomWords
import Haal.Experiment
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

mkWMethod :: Int -> WMethod
mkWMethod = WMethod . WMethodConfig

-- | The 'wmethodSuiteSize' function computes the size of the test suite for the W-method.
wmethodSuiteSize ::
    ( Automaton aut s i o
    , FiniteOrd i
    , FiniteOrd s
    , Eq o
    ) =>
    WMethod ->
    aut s i o ->
    Int
wmethodSuiteSize (WMethod (WMethodConfig{wmDepth = d})) aut = size
  where
    alphabet = Set.size $ inputs aut
    accessSeqs = Map.size $ accessSequences aut
    characterizingSet = Set.size $ globalCharacterizingSet aut
    transitionCover = accessSeqs * alphabet
    size = sum [transitionCover * (alphabet ^ n) * characterizingSet | n <- [0 .. d]]

-- | The 'wmethodSuite' function generates the test suite for the W-method and a new oracle.
wmethodSuite ::
    ( Automaton aut s i o
    , FiniteOrd i
    , FiniteOrd s
    , Eq o
    ) =>
    WMethod ->
    aut s i o ->
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
data RandomWMethodConfig = RandomWMethodConfig
    { rwmGen :: StdGen
    -- ^ The random number generator.
    , rwmLimit :: Int
    -- ^ The maximum number of random words to generate.
    , rwmLength :: Int
    -- ^ The maximum length of the random words.
    }
    deriving (Show, Eq)

-- | The 'RandomWMethod' type represents a random W-method equivalence oracle.
newtype RandomWMethod = RandomWMethod RandomWMethodConfig deriving (Show, Eq)

mkRandomWMethod :: StdGen -> Int -> Int -> RandomWMethod
mkRandomWMethod g l n = RandomWMethod (RandomWMethodConfig g n l)

-- | The 'randomWMethodSuite' function generates the test suite for the random W-method and a new oracle.
randomWMethodSuite ::
    forall i o s aut.
    ( Automaton aut s i o
    , FiniteOrd i
    , FiniteOrd s
    , Eq o
    ) =>
    RandomWMethod ->
    aut s i o ->
    (RandomWMethod, [[i]])
randomWMethodSuite (RandomWMethod (RandomWMethodConfig g wpr wl)) aut =
    let rorc = RandomWords (RandomWordsConfig{rwMaxLength = wl, rwMinLength = 1, rwLimit = wpr, rwGen = g})
        prefixes = Map.elems $ accessSequences aut
        vecSuffixes = Vec.fromList $ Set.toList $ globalCharacterizingSet aut

        (RandomWords roc', wordBatches) = List.mapAccumL testSuite rorc (replicate (length prefixes) (undefined :: aut s i o))
        flatWords = concat wordBatches

        (gen'', gen''') = split (rwGen roc')
        samples = length prefixes * wpr
        randomSuffixes = take samples $ randomRs (0, Vec.length vecSuffixes - 1) gen''
        suffixes = map (vecSuffixes Vec.!) randomSuffixes

        suite = [prefix ++ rand ++ suffix | prefix <- prefixes, rand <- flatWords, suffix <- suffixes]
     in (mkRandomWMethod gen''' wpr wl, suite)

instance EquivalenceOracle RandomWMethod where
    testSuite = randomWMethodSuite
