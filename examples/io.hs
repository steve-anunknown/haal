-- we will attempt to reproduce the `div.hs` learning experiment,
-- but this time, instead of using a haskell function as a SUL,
-- we will use an actual program that performs IO.
-- the program reads an integer from stdin and prints whether it is
-- divisible by 3, so its output alphabet is just bool.
-- the input alphabet is binary, as in `div.hs`.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Haal.BlackBox (SUL (..))
import Haal.EquivalenceOracle.WpMethod (WpMethodConfig (..), mkWpMethod)
import Haal.Experiment (measuredExperiment, runExperimentT)
import Haal.Learning.LMstar (LMstarConfig (Star), mkLMstar)
import Haal.Statistics (statistics)
import System.Process (readProcess)

-- Note that this is relative to the project root. Otherwise
-- the executable will not be found. Build it first with
--   ghc examples/divisible3.hs
source :: FilePath
source = "./examples/divisible3"

data Binary = B0 | B1 deriving (Show, Eq, Ord, Enum, Bounded)

-- the bits seen so far are read as a binary number, most significant
-- bit first. the history is stored newest bit first, so the head of the
-- list is the least significant bit.
convert :: [Binary] -> Integer
convert = foldr (\b acc -> toInteger (fromEnum b) + 2 * acc) 0

-- ask the external program about the number the bits represent
askProgram :: [Binary] -> IO Bool
askProgram bits = read <$> readProcess source [] (show (convert bits) ++ "\n")

-- the program itself is stateless, so the SUL keeps the inputs it has
-- received since the last reset and queries the program with all of them
-- on every step.
data Program i o = Program ([i] -> IO o) [i]

instance SUL Program IO where
    step (Program f buf) x = do
        let buf' = x : buf
        o <- f buf'
        return (Program f buf', o)
    reset (Program f _) = return (Program f [])

sul :: Program Binary Bool
sul = Program askProgram []

main :: IO ()
main = do
    oracle <- either fail return (mkWpMethod (WpMethodConfig 3))
    let learner = mkLMstar Star
    (theModel, theStats) <- runExperimentT (measuredExperiment statistics learner oracle) sul
    putStrLn "Learning Experiment"
    putStrLn "==================="
    putStrLn "System Under Learning: ./examples/divisible3"
    putStrLn $ "Learned Model: " ++ show theModel
    putStrLn $ "Experiment Statistics: " ++ show theStats
