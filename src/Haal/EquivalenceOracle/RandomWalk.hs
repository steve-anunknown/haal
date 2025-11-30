-- | This module implements the Random Walk equivalence oracle.
module Haal.EquivalenceOracle.RandomWalk (
    RandomWalk (..),
    RandomWalkConfig (..),
    mkRandomWalk,
)
where

import qualified Data.Set as Set
import qualified Data.Vector as V
import Haal.BlackBox
import Haal.Experiment
import System.Random (
    Random (randomR, randomRs),
    RandomGen (split),
    StdGen,
 )

-- | The 'RandomWalkConfig' data type represents the configuration for an instance of the 'RandomWalk' algorithm.
data RandomWalkConfig = RandomWalkConfig
    { rwlGen :: StdGen
    , rwlMaxSteps :: Int
    , rwlRestart :: Double
    }
    deriving (Eq, Show)

-- | The 'RandomWalk' data type is just a wrapper around the config.
newtype RandomWalk = RandomWalk RandomWalkConfig deriving (Show, Eq)

-- | Constructor for a 'RandomWalk' value.
mkRandomWalk :: RandomWalkConfig -> RandomWalk
mkRandomWalk = RandomWalk

-- | Generates a random walk for the automaton.
randomWalkSuite :: (FiniteOrd a) => RandomWalk -> sul a o -> (RandomWalk, [[a]])
randomWalkSuite (RandomWalk (RandomWalkConfig{rwlGen = g, rwlMaxSteps = maxS, rwlRestart = restartP})) aut =
    let (g1, g2) = split g
        alphabet = V.fromList . Set.toList $ inputs aut
        randomInputs = take maxS $ randomRs (0, V.length alphabet - 1) g1
        inputSequence = map (alphabet V.!) randomInputs
        (inputSequence', g3) = splitWithProbability g2 restartP inputSequence
        oracle' = mkRandomWalk (RandomWalkConfig g3 maxS restartP)
     in (oracle', inputSequence')

splitWithProbability :: StdGen -> Double -> [a] -> ([[a]], StdGen)
splitWithProbability generator p symbols = go generator symbols [] []
  where
    go g [] acc word = (word : acc, g)
    go g (x : xs) acc word =
        let (r, g') = randomR (0.0, 1.0) g
         in if r < p && not (null word)
                then go g' xs (word : acc) [x]
                else go g' xs acc (x : word)

instance EquivalenceOracle RandomWalk where
    testSuite = randomWalkSuite
