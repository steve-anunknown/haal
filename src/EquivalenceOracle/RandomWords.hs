-- | This module implements a simple random words equivalence oracle.
module EquivalenceOracle.RandomWords (
    RandomWords (..),
)
where

import BlackBox (inputs)
import qualified Data.Set as Set
import Experiment (EquivalenceOracle, testSuite)
import System.Random

data RandomWords = RandomWords {gen :: StdGen, limit :: Int, minLength :: Int, maxLength :: Int}
    deriving (Eq, Show)

generateRandomWords ::
    (Ord i, Bounded i, Enum i) =>
    RandomWords ->
    aut i o ->
    (RandomWords, [[i]])
generateRandomWords (RandomWords{gen = generator, limit = count, minLength = minL, maxLength = maxL}) aut =
    (oracle, suite)
  where
    alpha = Set.toList $ inputs aut

    go 0 g acc = (reverse acc, g)
    go n g acc =
        let (len, g1) = randomR (minL, maxL) g
            (word, g2) = generateWord g1 len alpha
         in go (n - 1) g2 (word : acc)

    generateWord :: StdGen -> Int -> [i] -> ([i], StdGen)
    generateWord g 0 _ = ([], g)
    generateWord g n a =
        let (ix, g1) = randomR (0, length a - 1) g
            sym = a !! ix
            (rest, g2) = generateWord g1 (n - 1) a
         in (sym : rest, g2)

    (suite, stdgen) = go count generator []
    oracle = RandomWords{gen = stdgen, limit = count, minLength = minL, maxLength = maxL}

instance EquivalenceOracle RandomWords where
    testSuite = generateRandomWords
