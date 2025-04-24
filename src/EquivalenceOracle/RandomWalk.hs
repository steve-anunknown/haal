module EquivalenceOracle.RandomWalk (
    RandomWalk (..),
    RandomWalkConfig (..),
)
where

import BlackBox
import qualified Data.Set as Set
import qualified Data.Vector as V
import Experiment
import System.Random (Random (randomR), RandomGen (split), StdGen, randomRs)

data RandomWalkConfig = RandomWalkConfig
    { rwlGen :: StdGen
    , rwlMaxSteps :: Int
    , rwlRestart :: Double
    }
    deriving (Eq, Show)

newtype RandomWalk = RandomWalk RandomWalkConfig deriving (Show, Eq)

-- | Generates a random walk for the automaton.
randomWalkSuite :: (Ord a, Bounded a, Enum a) => RandomWalk -> sul a o -> (RandomWalk, [[a]])
randomWalkSuite (RandomWalk (RandomWalkConfig{rwlGen = g, rwlMaxSteps = maxS, rwlRestart = restartP})) aut =
    let (g1, g2) = split g
        alphabet = V.fromList . Set.toList $ inputs aut
        randomInputs = take maxS $ randomRs (0, V.length alphabet - 1) g1
        inputSequence = map (alphabet V.!) randomInputs
        (inputSequence', g3) = splitWithProbability g2 restartP inputSequence
        oracle' = RandomWalkConfig{rwlGen = g3, rwlMaxSteps = maxS, rwlRestart = restartP}
     in (RandomWalk oracle', inputSequence')

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
