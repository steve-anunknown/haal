{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module implements a simple deterministic finite automaton (DFA).
module DFA (
    DFAState (..),
    DFA (..),
    dfaStep,
    dfaWalk,
)
where

import qualified Data.List as List

{- | The DFAState type can be parameterized by two types, representing
the accepting and rejecting states.
-}
data DFAState a b = Accepting a | Rejecting b deriving (Show, Eq)

{- | The DFA type represents a deterministic finite automaton. It is
parameterized by the input type and the types for accepting and rejecting states.
-}
data DFA input acc rej = DFA
    { dfaDelta :: DFAState acc rej -> input -> DFAState acc rej
    , dfaCurrentS :: DFAState acc rej
    , dfaInitialS :: DFAState acc rej
    }

{- | Performs a step of the DFA, transitioning to the next state based on the input.
The function returns the updated DFA and a boolean indicating whether the current state
is accepting.
-}
dfaStep :: DFA input acc rej -> input -> (DFA input acc rej, Bool)
dfaStep DFA{dfaDelta = delta, dfaCurrentS = curr, dfaInitialS = initial} i =
    ( DFA
        { dfaDelta = delta
        , dfaCurrentS = nextS
        , dfaInitialS = initial
        }
    , result
    )
  where
    nextS = delta curr i
    result = case nextS of
        Accepting _ -> True
        _ -> False

{- | Walks through a sequence of inputs, updating the DFA state and returning
a list of booleans indicating whether each state is accepting.
-}
dfaWalk :: (Traversable t) => DFA input acc rej -> t input -> (DFA input acc rej, t Bool)
dfaWalk = List.mapAccumL dfaStep
