{-# LANGUAGE ScopedTypeVariables #-}

import BlackBox -- Assuming this exposes the Automaton and SUL you need
import Criterion.Main
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Random (mkStdGen)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck (Arbitrary (..))
import WMethod (WMethod (..), wmethod)
import Utils 

-- Generate deterministic automaton from a seed
generateAutomaton :: Int -> Mealy Input Output State 
generateAutomaton seed = unGen arbitrary (mkStdGen seed) 30

main :: IO ()
main = do
    let aut = generateAutomaton 42 -- fixed seed for repeatability
        oracle = WMethod 3

    defaultMain
        [ bench "W Method depth 3 on generated automaton" $
            nf (\() -> wmethod oracle aut aut) ()
        ]
