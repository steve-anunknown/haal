-- | This module provides serialization of Mealy automata to DOT format,
-- following the convention used by AALpy and LearnLib.
module Haal.Dot
    ( mealyToDot
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Haal.Automaton.MealyAutomaton (MealyAutomaton, mealyTransitions)
import Haal.BlackBox (Automaton (..), FiniteOrd, initial)

{- | Serialize a 'MealyAutomaton' to a DOT format string.

  The output follows the AALpy\/LearnLib convention:

  * States are rendered as @s0@, @s1@, … in ascending order, with their
    'show' representation as the node label.
  * Edge labels have the form @\"input\/output\"@.
  * The initial state is indicated by a @__start0@ dummy node.

  The 'Show' instances for @i@ and @o@ must not produce double-quote
  characters, as those are not escaped in the label strings.
-}
mealyToDot
    :: (FiniteOrd s, FiniteOrd i, Show s, Show i, Show o)
    => MealyAutomaton s i o
    -> String
mealyToDot m =
    unlines $
        ["digraph haal {"]
        ++ map nodeDecl (zip [0 :: Int ..] sortedStates)
        ++ map edgeDecl (Map.toList trans)
        ++ [ "\t__start0 [label=\"\" shape=none];"
           , "\t__start0 -> " ++ nodeId (initial m) ++ " [label=\"\"];"
           , "}"
           ]
  where
    sortedStates = Set.toAscList (states m)
    stateIndex   = Map.fromList (zip sortedStates [0 :: Int ..])
    trans        = mealyTransitions m

    nodeId s     = "s" ++ show (stateIndex Map.! s)

    nodeDecl (i, s) =
        "\ts" ++ show i ++ " [label=\"" ++ show s ++ "\"];"

    edgeDecl ((s, i), (s', o)) =
        "\t" ++ nodeId s ++ " -> " ++ nodeId s'
        ++ " [label=\"" ++ show i ++ "/" ++ show o ++ "\"];"
