{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- | Copyright: (c) 2026 Joshua Moerman, Open Universiteit
SPDX-License-Identifier: BSD-3-Clause

Implementations of the two automata families in Hopcrofts minimisation
paper from 1971. Note that these did not originally have an initial state,
so we added one. Also these are DFAs, but we can implement them as Mealy.

"An n log n algorithm for minimizing states in a finite automaton", 1971,
John Hopcroft.
https://doi.org/10.1016/B978-0-12-417750-5.50022-1
-}
module Bench.Hopcroft (
    -- * Example automata
    hopcroftA,
    hopcroftB,
    hopcroftB2,

    -- * Input and output
    Input (..),
    Output (..),
) where

import Control.DeepSeq
import Data.Set qualified as Set
import GHC.Generics (Generic)

import Haal.Automaton.MealyAutomaton

{- | Hopcrofts automata have a single symbol, we add another to convert
the DFA to a Mealy machine.
-}
data Input
    = A
    | Get
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData)

{- | The `Get` symbol returns the output as specified by the DFA (either
`Accept` or `Reject`), and it outputs `None` otherwise.
-}
data Output
    = None
    | Accept
    | Reject
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData)

{- | Long chain of states, of which the last is accepting. For
the input @n@, the number of states is @n@.
-}
hopcroftA :: Int -> MealyAutomaton Int Input Output
hopcroftA n = mkMealyAutomaton delta lambda sts initS
  where
    delta 0 _ = 0
    delta i A = i - 1
    delta i Get = i
    lambda _ A = None
    lambda 0 Get = Accept
    lambda _ Get = Reject
    sts = Set.fromList [0 .. n - 1]
    initS = n - 1

{- | The B family of automata in Hopcrofts paper. For the input
@n@, the number of states is @4n@. Not all states are reachable.
This is an example where the "split the smaller half" make a big
algorithmic improvement.
-}
hopcroftB :: Int -> MealyAutomaton Int Input Output
hopcroftB n = mkMealyAutomaton delta lambda sts initS
  where
    delta i A
        | 0 <= i && i < n = 2 * n + 2 * i
        | n <= i && i < 2 * n = 2 * i - 2 * n
        | 2 * n <= i && i < 4 * n = 2 * i - 4 * n
        | otherwise = error "hopcroftB out of bounds"
    delta i Get = i
    lambda _ A = None
    lambda i Get
        | 0 <= i && i < 2 * n = Accept
        | 2 * n <= i && i < 4 * n = Reject
        | otherwise = error "hopcroftB out of bounds"
    sts = Set.fromList [0 .. 4 * n - 1]
    initS = 0

{- | Variation of `hopcroftB` so that it is reachable. This does not
correspond to a DFA anymore.
-}
hopcroftB2 :: Int -> MealyAutomaton Int Input Output
hopcroftB2 n = mkMealyAutomaton delta lambda sts initS
  where
    delta i A
        | 0 <= i && i < n = 2 * n + 2 * i
        | n <= i && i < 2 * n = 2 * i - 2 * n
        | 2 * n <= i && i < 4 * n = 2 * i - 4 * n
        | otherwise = error "hopcroftB2 out of bounds"
    delta 0 Get = 4 * n - 1
    delta i Get = i - 1
    lambda _ A = None
    lambda i Get
        | 0 <= i && i < 2 * n = Accept
        | 2 * n <= i && i < 4 * n = Reject
        | otherwise = error "hopcroftB2 out of bounds"
    sts = Set.fromList [0 .. 4 * n - 1]
    initS = 0
