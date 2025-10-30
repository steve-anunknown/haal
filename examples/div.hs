{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Haal.Automaton.MealyAutomaton
import Haal.BlackBox (SUL (..), StateID)
import Haal.EquivalenceOracle.WpMethod (WpMethod, mkWpMethod)
import Haal.Experiment
import Haal.Learning.LMstar (LMstar, LMstarConfig (Star), mkLMstar)

-- main logic
divisible :: Integer -> Bool
divisible n = n `mod` (3 :: Integer) == 0

-- suppose we want to construct an automaton representation of this program using model learning

-- first of all, we choose a representation for the inputs of the program.
-- one option is to use decimal digits, another is to use binary digits.
-- the reason why we use a representation is because we cannot use the existing integers
-- as they are infinite. we cannot use an infinite input alphabet for our model, because
-- we can't possibly define its behaviours for all symbols of the alphabet, that is all the
-- integers. we have to code the integers.

data Binary = B0 | B1 deriving (Show, Eq, Ord, Enum, Bounded)

-- now we are in the position to use binary digits to construct integers.
-- we need a mapper that maps from binary digits to integers that the program can actually use

convert :: (Num a) => [Binary] -> a
convert [] = 0
convert [B0] = 0
convert [B1] = 1
convert (b : bs) = convert [b] + 2 * convert bs

-- now, remember that the only notion of SUL that is defined in the library is
-- a typeclass that states what functions must be implemented, like a java interface.
-- in order to create our system, we have to define it as a type and

data Program i o = Program
    { theStep :: i -> (Program i o, o)
    , theReset :: Program i o
    , buffer :: [i]
    }

instance Haal.BlackBox.SUL Program i o where
    step = theStep
    reset = theReset

wrapped :: [Binary] -> Bool
wrapped [] = False
wrapped bits = divisible $ convert bits

wrappedProg :: [Binary] -> Program Binary Bool
wrappedProg buf =
    Program
        { theStep = \x ->
            let newBuf = x : buf
             in (wrappedProg newBuf, wrapped newBuf)
        , theReset = wrappedProg []
        , buffer = buf
        }

wrappedP :: Program Binary Bool
wrappedP = wrappedProg []

learner :: LMstar Binary Bool
learner = mkLMstar Star

oracle :: WpMethod
oracle = mkWpMethod 3

sul :: Program Binary Bool
sul = wrappedP

exper :: Experiment (Program Binary Bool) (MealyAutomaton StateID Binary Bool, Statistics MealyAutomaton StateID Binary Bool)
exper = experiment learner oracle

theModel :: MealyAutomaton StateID Binary Bool
theStats :: Statistics MealyAutomaton StateID Binary Bool
(theModel, theStats) = runExperiment exper sul

main :: IO ()
main = do
    putStrLn "Learning Experiment"
    putStrLn "==================="
    putStrLn $ "System Under Learning: \\x -> x `mod` 3 == 0"
    putStrLn $ "Learned Model: " ++ show theModel
    putStrLn $ "Experiment Statistics: " ++ show theStats
