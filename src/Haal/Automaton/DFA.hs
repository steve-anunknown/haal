{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module implements a simple deterministic finite automaton (DFA).
module Haal.Automaton.DFA (
    DFA,
    mkDFA,
)
where

import qualified Data.Set as Set
import Haal.Automaton.MooreAutomaton

-- | 'DFA' is just a synonym for a 'MooreAutomaton' with 'Bool' type of output'.
type DFA state input = MooreAutomaton state input Bool

mkDFA :: (s -> i -> s) -> (s -> Bool) -> Set.Set s -> s -> DFA s i
mkDFA = mkMooreAutomaton
