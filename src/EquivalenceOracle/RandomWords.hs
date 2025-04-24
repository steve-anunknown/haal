-- | This module implements a simple random words equivalence oracle.
module EquivalenceOracle.RandomWords (
    RandomWords (..),
    RandomWordsConfig (..),
)
where

import BlackBox (inputs)
import Control.Monad (replicateM)
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Vector as V
import Experiment (EquivalenceOracle, testSuite)
import System.Random

-- RandomWords.hs or a shared Configs.hs if you prefer
data RandomWordsConfig = RandomWordsConfig
    { rwGen :: StdGen
    , rwLimit :: Int
    , rwMinLength :: Int
    , rwMaxLength :: Int
    }
    deriving (Eq, Show)

newtype RandomWords = RandomWords RandomWordsConfig deriving (Show, Eq)

generateRandomWords :: (Ord a, Bounded a, Enum a) => RandomWords -> sul a o -> (RandomWords, [[a]])
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
        let alphaVec = V.fromList . Set.toList $ inputs aut
            (ranWords, finalGen) = runState (replicateM count (genWord alphaVec)) generator
            config = RandomWordsConfig{rwGen = finalGen, rwLimit = count, rwMinLength = minL, rwMaxLength = maxL}
         in (RandomWords config, ranWords)
      where
        genWord alphaVec = do
            len <- state $ randomR (minL, maxL)
            replicateM len (state $ \g -> let (ix, g') = randomR (0, V.length alphaVec - 1) g in (alphaVec V.! ix, g'))

instance EquivalenceOracle RandomWords where
    testSuite = generateRandomWords
