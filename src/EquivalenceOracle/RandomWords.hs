-- | This module implements a simple random words equivalence oracle.
module EquivalenceOracle.RandomWords (
    RandomWords (..),
)
where

import BlackBox (inputs)
import Control.Monad (replicateM)
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Vector as V
import Experiment (EquivalenceOracle, testSuite)
import System.Random

data RandomWords = RandomWords {gen :: StdGen, limit :: Int, minLength :: Int, maxLength :: Int}
    deriving (Eq, Show)

generateRandomWords :: (Ord a, Bounded a, Enum a) => RandomWords -> sul a o -> (RandomWords, [[a]])
generateRandomWords (RandomWords{gen = generator, limit = count, minLength = minL, maxLength = maxL}) aut =
    let alphaVec = V.fromList . Set.toList $ inputs aut
        (ranWords, finalGen) = runState (replicateM count (genWord alphaVec)) generator
        oracle = RandomWords{gen = finalGen, limit = count, minLength = minL, maxLength = maxL}
     in (oracle, ranWords)
  where
    genWord alphaVec = do
        len <- state $ randomR (minL, maxL)
        replicateM len (state $ \g -> let (ix, g') = randomR (0, V.length alphaVec - 1) g in (alphaVec V.! ix, g'))

instance EquivalenceOracle RandomWords where
    testSuite = generateRandomWords
