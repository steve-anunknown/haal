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

data RandomWalkConfig = RandomWalkConfig
    { rwlGen :: StdGen
    , rwlMaxSteps :: Int
    , rwlRestart :: Double
    }
    deriving (Eq, Show)

newtype RandomWalk = RandomWalk RandomWalkConfig deriving (Show, Eq)

mkRandomWalk :: StdGen -> Int -> Double -> RandomWalk
mkRandomWalk gen maxS restartP =
    RandomWalk
        RandomWalkConfig
            { rwlGen = gen
            , rwlMaxSteps = maxS
            , rwlRestart = restartP
            }

-- | Generates a random walk for the automaton.
randomWalkSuite :: (FiniteOrd a) => RandomWalk -> sul a o -> (RandomWalk, [[a]])
randomWalkSuite (RandomWalk (RandomWalkConfig{rwlGen = g, rwlMaxSteps = maxS, rwlRestart = restartP})) aut =
    let (g1, g2) = split g
        alphabet = V.fromList . Set.toList $ inputs aut
        randomInputs = take maxS $ randomRs (0, V.length alphabet - 1) g1
        inputSequence = map (alphabet V.!) randomInputs
        (inputSequence', g3) = splitWithProbability g2 restartP inputSequence
        oracle' = mkRandomWalk g3 maxS restartP
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
