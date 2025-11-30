-- | This module implements a simple random words equivalence oracle.
module Haal.EquivalenceOracle.RandomWords (
    RandomWords (..),
    RandomWordsConfig (..),
    mkRandomWords,
)
where

import Control.Monad (replicateM)
import Control.Monad.State (MonadState (state), runState)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Haal.BlackBox
import Haal.Experiment
import System.Random (Random (randomR), StdGen)

-- | Data type that represents the configuration for an instance of the 'RandomWords' algorithm.
data RandomWordsConfig = RandomWordsConfig
    { rwGen :: StdGen
    , rwLimit :: Int
    , rwMinLength :: Int
    , rwMaxLength :: Int
    }
    deriving (Eq, Show)

-- | The 'RandomWords' type is just a wrapper around the respective config type.
newtype RandomWords = RandomWords RandomWordsConfig deriving (Show, Eq)

-- | Constructor for a 'RandomWords' data type.
mkRandomWords :: StdGen -> Int -> Int -> Int -> RandomWords
mkRandomWords gen limit minL maxL =
    RandomWords
        RandomWordsConfig
            { rwGen = gen
            , rwLimit = limit
            , rwMinLength = minL
            , rwMaxLength = maxL
            }

-- | Return the test suite of the 'RandomWords' algorithm.
generateRandomWords :: (FiniteOrd a) => RandomWords -> sul a o -> (RandomWords, [[a]])
generateRandomWords
    ( RandomWords
            RandomWordsConfig
                { rwGen = generator
                , rwLimit = count
                , rwMinLength = minL
                , rwMaxLength = maxL
                }
        )
    aut =
        let (ranWords, finalGen) = runState (replicateM count genWord) generator
            oracle = mkRandomWords finalGen count minL maxL
         in (oracle, ranWords)
      where
        alphaVec = V.fromList . Set.toList $ inputs aut
        alphaLen = V.length alphaVec - 1
        genWord = do
            len <- state $ randomR (minL, maxL)
            replicateM len (state $ \g -> let (ix, g') = randomR (0, alphaLen) g in (alphaVec V.! ix, g'))

instance EquivalenceOracle RandomWords where
    testSuite = generateRandomWords
