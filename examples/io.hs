-- we will attempt to reproduce the `div.hs` learning experiment,
-- but this time, instead of using a haskell function as a SUL,
-- we will use an actual program that performs IO, whose input and
-- output alphabet we know.
-- the output alphabet is just bool
-- the input alphabet is binary
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Functor ((<&>))
import Haal.Automaton.MealyAutomaton
import Haal.BlackBox
import Haal.EquivalenceOracle.WpMethod
import Haal.Experiment
import Haal.Learning.LMstar
import System.Process (readProcess)

-- Note that this is relative to the project root. Otherwise
-- the executable will not be found
source :: String
source = "./src/divisible3"

inputMap :: Int -> String
inputMap num = show num ++ "\n"

innerQuery :: String -> IO String
innerQuery = readProcess source []

outputMap :: String -> Bool
outputMap = read

query :: Int -> IO Bool
query = (<&> outputMap) . innerQuery . inputMap

data Binary = B0 | B1 deriving (Show, Eq, Ord, Enum, Bounded)

-- now we are in the position to use binary digits to construct integers.
-- we need a mapper that maps from binary digits to integers that the program can actually use

convert :: (Num a) => [Binary] -> a
convert [] = 0
convert [B0] = 0
convert [B1] = 1
convert (b : bs) = convert [b] + 2 * convert bs

data Program i o = Program
    { theStep :: i -> IO (Program i o, o)
    , theReset :: IO (Program i o)
    , buffer :: [i]
    }

-- fix from here on downwards

instance Haal.BlackBox.SUL Program IO Binary Bool where
    step = theStep
    reset = theReset

wrapped :: [Binary] -> IO Bool
wrapped = query . convert

mkProg :: [Binary] -> Program Binary Bool
mkProg buf =
    Program
        { theStep = \x -> do
            let newBuf = x : buf
            o <- wrapped newBuf
            return (mkProg newBuf, o)
        , theReset = return (mkProg [])
        , buffer = buf
        }

-- construct a sul with an empty buffer
sul :: Program Binary Bool
sul = mkProg []

learner :: LMstar Binary Bool
learner = mkLMstar Star

oracle :: WpMethod
oracle = mkWpMethod 3

exper ::
    ExperimentT
        (Program Binary Bool)
        IO
        ( MealyAutomaton
            StateID
            Binary
            Bool
        , Statistics
            MealyAutomaton
            StateID
            Binary
            Bool
        )
exper = experiment learner oracle


main :: IO ()
main = do
    (theModel, theStats) <- runExperimentT exper sul
    putStrLn "Learning Experiment"
    putStrLn "==================="
    putStrLn "System Under Learning: \\x -> x `mod` 3 == 0"
    putStrLn $ "Learned Model: " ++ show theModel
    putStrLn $ "Experiment Statistics: " ++ show theStats
