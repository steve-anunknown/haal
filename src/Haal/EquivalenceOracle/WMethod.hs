{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements the W-method equivalence oracle.
module Haal.EquivalenceOracle.WMethod (
    WMethod,
    WMethodConfig (..),
    wmethodSuiteSize,
    RandomWMethod,
    RandomWMethodConfig (..),
    mkWMethod,
    mkRandomWMethod,
) where

import Control.Monad (replicateM)
import Control.Monad.State (runState, state)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Haal.BlackBox
import Haal.Experiment
import System.Random (Random (randomR), StdGen)

-- | The 'WMethodConfig' type is used to configure the W-method equivalence oracle.
newtype WMethodConfig = WMethodConfig
    { wmDepth :: Int
    -- ^ The number of extra states beyond the hypothesis to account for.
    }
    deriving (Show, Eq)

-- | The 'WMethod' type represents the W-method equivalence oracle.
newtype WMethod = WMethod WMethodConfig deriving (Show, Eq)

-- | Constructor for a 'WMethod' value.
mkWMethod :: WMethodConfig -> Either String WMethod
mkWMethod cfg
    | 0 <= wmDepth cfg = Right (WMethod cfg)
    | otherwise = Left ("Must be 0 <= wmDepth but got " ++ show cfg)

-- | The 'wmethodSuiteSize' function computes the size of the test suite for the W-method.
wmethodSuiteSize ::
    ( Automaton aut s
    , FiniteOrd i
    , FiniteOrd s
    , Eq o
    ) =>
    WMethod ->
    aut s i o ->
    Int
wmethodSuiteSize (WMethod (WMethodConfig d)) aut = size
  where
    alphabet = Set.size $ inputs aut
    accessSeqs = Map.size $ accessSequences aut
    characterizingSet = Set.size $ testSuffixes (globalCharacterizingSet aut)
    transitionCover = accessSeqs * alphabet
    size = sum [transitionCover * (alphabet ^ n) * characterizingSet | n <- [0 .. d]]

-- | The 'wmethodSuite' function generates the test suite for the W-method and a new oracle.
wmethodSuite ::
    ( Automaton aut s
    , FiniteOrd i
    , FiniteOrd s
    , Eq o
    ) =>
    WMethod ->
    aut s i o ->
    (WMethod, [[i]])
wmethodSuite wm@(WMethod (WMethodConfig d)) aut = (wm, suite)
  where
    alphabet = Set.toList $ inputs aut
    accessSeqs = accessSequences aut
    characterizingSet = Set.toList $ testSuffixes (globalCharacterizingSet aut)
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

-- | Constructor for a 'RandomWMethod' value.
mkRandomWMethod :: RandomWMethodConfig -> Either String RandomWMethod
mkRandomWMethod cfg
    | 0 <= rwmLimit cfg && 0 <= rwmLength cfg = Right (RandomWMethod cfg)
    | otherwise = Left ("Must be 0 <= rwmLimit and 0 <= rwmLength but got " ++ show cfg)

-- | The 'randomWMethodSuite' function generates the test suite for the random W-method and a new oracle.
randomWMethodSuite ::
    forall i o s aut.
    ( Automaton aut s
    , FiniteOrd i
    , FiniteOrd s
    , Eq o
    ) =>
    RandomWMethod ->
    aut s i o ->
    (RandomWMethod, [[i]])
randomWMethodSuite (RandomWMethod (RandomWMethodConfig g wpr wl)) aut =
    let prefixes = Map.elems $ accessSequences aut
        suffixes = Set.toList $ testSuffixes (globalCharacterizingSet aut)
        alphaVec = Vec.fromList . Set.toList $ inputs aut
        genWord =
            if wl == 0
                then return []
                else do
                    len <- state $ randomR (1, wl)
                    replicateM
                        len
                        ( state $
                            \gen ->
                                let (ix, gen') = randomR (0, Vec.length alphaVec - 1) gen
                                 in (alphaVec Vec.! ix, gen')
                        )
        (ranWords, g') = runState (replicateM wpr genWord) g
        suite = [prefix ++ rand ++ suffix | prefix <- prefixes, rand <- ranWords, suffix <- suffixes]
     in (RandomWMethod (RandomWMethodConfig g' wpr wl), suite)

instance EquivalenceOracle RandomWMethod where
    testSuite = randomWMethodSuite

{- | The suffixes that test words end with: the given characterizing set, or
just the empty word when that set is empty. A hypothesis with a single state
has an empty characterizing set (there is nothing to distinguish), and without
the empty word it would get no test words at all, so it would be accepted
without any testing. Ending a test word with the empty word still checks the
outputs along the rest of the word.
-}
testSuffixes :: Set.Set [i] -> Set.Set [i]
testSuffixes w
    | Set.null w = Set.singleton []
    | otherwise = w
