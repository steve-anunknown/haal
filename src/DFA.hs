{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module DFA (
    DFAState (..),
    DFA (..),
    dfaStep,
    dfaWalk,
)
where

import qualified Data.List as List

data DFAState a b = Accepting a | Rejecting b deriving (Show, Eq)

data DFA input acc rej = DFA
    { dfaDelta :: DFAState acc rej -> input -> DFAState acc rej
    , dfaCurrentS :: DFAState acc rej
    , dfaInitialS :: DFAState acc rej
    }

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

dfaWalk :: (Traversable t) => DFA input acc rej -> t input -> (DFA input acc rej, t Bool)
dfaWalk = List.mapAccumL dfaStep

