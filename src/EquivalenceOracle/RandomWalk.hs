module EquivalenceOracle.RandomWalk (
    RandomWalk (..),
)
where

import BlackBox
import qualified Data.List as List
import qualified Data.Set as Set
import Experiment
import System.Random

data RandomWalk = RandomWalk {gen :: StdGen, maxSteps :: Int, restart :: Double} deriving (Show, Eq)

-- In order to implement the random walk oracle, the test suite
-- is going to be generated the following way: Generate a list
-- of length maxSteps with random symbols and then split it in
-- random places.

randomWalkSuite ::
    (Bounded i, Enum i, Ord i) =>
    RandomWalk ->
    aut i o ->
    (RandomWalk, [[i]])
randomWalkSuite (RandomWalk{gen = g, maxSteps = maxS, restart = restartP}) aut =
    let (g1, g2) = split g
        alphabet = Set.toList $ inputs aut
        alphaLen = length alphabet
        randomInputs = take maxS $ List.unfoldr (Just . randomR (0, alphaLen - 1)) g1
        inputSequence = map (alphabet !!) randomInputs
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
