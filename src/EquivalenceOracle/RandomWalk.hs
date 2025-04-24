module EquivalenceOracle.RandomWalk (
    RandomWalk (..),
)
where

import BlackBox
import qualified Data.Set as Set
import qualified Data.Vector as V
import Experiment
import System.Random (Random (randomR), RandomGen (split), StdGen, randomRs)

data RandomWalk = RandomWalk {gen :: StdGen, maxSteps :: Int, restart :: Double} deriving (Show, Eq)

-- In order to implement the random walk oracle, the test suite
-- is going to be generated the following way: Generate a list
-- of length maxSteps with random symbols and then split it in
-- random places.

randomWalkSuite :: (Ord a, Bounded a, Enum a) => RandomWalk -> sul a o -> (RandomWalk, [[a]])
randomWalkSuite (RandomWalk{gen = g, maxSteps = maxS, restart = restartP}) aut =
    let (g1, g2) = split g
        alphabet = V.fromList . Set.toList $ inputs aut
        randomInputs = take maxS $ randomRs (0, V.length alphabet - 1) g1
        inputSequence = map (alphabet V.!) randomInputs
        (inputSequence', g3) = splitWithProbability g2 restartP inputSequence
        oracle' = RandomWalk{gen = g3, maxSteps = maxS, restart = restartP}
     in (oracle', inputSequence')

splitWithProbability :: StdGen -> Double -> [a] -> ([[a]], StdGen)
splitWithProbability generator p symbols = go generator symbols [] []
  where
    go g [] acc word = (reverse (reverse word : acc), g)
    go g (x : xs) acc word =
        let (r, g') = randomR (0.0, 1.0) g
         in if r < p && not (null word)
                then go g' xs (reverse word : acc) [x]
                else go g' xs acc (x : word)

instance EquivalenceOracle RandomWalk where
    testSuite = randomWalkSuite
