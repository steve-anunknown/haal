{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements the WpMethod.
module EquivalenceOracle.WpMethod (
    WpMethod (..),
    WpMethodConfig (..),
    wpmethodSuiteSize,
) where

import BlackBox
import Control.Monad (replicateM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Experiment (EquivalenceOracle (..))

newtype WpMethodConfig = WpMethodConfig
    { wpDepth :: Int
    -- ^ The maximum depth of the WpMethod.
    }
    deriving (Show, Eq)

-- | Type for the WpMethod. It simply wraps the depth of the method.
newtype WpMethod = WpMethod WpMethodConfig deriving (Eq, Show)

-- | The 'wpmethodSuiteSize' returns the nunmber of test cases in the test suite of WpMethod
wpmethodSuiteSize :: a
wpmethodSuiteSize = error "todo"

-- | Returns the test suite for the WpMethod.
wpmethodSuite ::
    forall aut i o s.
    ( Automaton aut s
    , Ord i
    , Bounded i
    , Enum i
    , Ord s
    , Bounded s
    , Enum s
    , Eq o
    ) =>
    WpMethod ->
    aut i o ->
    (WpMethod, [[i]])
wpmethodSuite wpm@(WpMethod (WpMethodConfig{wpDepth = d})) aut = (wpm, suite)
  where
    alphabet = inputs aut
    stateCover = accessSequences aut
    localSuf =
        Map.fromList
            [ (st, localCharacterizingSet aut st) | st <- Set.toList $ states aut
            ]
    globalSuf = globalCharacterizingSet aut

    transitionCover =
        [ acc ++ [a]
        | acc <- Map.elems stateCover
        , a <- Set.toList alphabet
        ]
    difference =
        Set.fromList (Map.elems stateCover)
            `Set.difference` Set.fromList transitionCover

    firstPhase =
        concat
            [ [ acc ++ middle ++ suf
              | acc <- Map.elems stateCover
              , suf <- Set.toList globalSuf
              ]
            | fixed <- [0 .. d]
            , middle <- replicateM fixed $ Set.toList alphabet
            ]

    secondPhase =
        concat
            [ [ acc ++ middle ++ suf
              | acc <- Set.toList difference
              , suf <- Set.toList $ localSuf Map.! current (fst (walk aut (acc ++ middle)))
              ]
            | fixed <- [0 .. d]
            , middle <- replicateM fixed $ Set.toList alphabet
            ]

    suite = firstPhase ++ secondPhase

instance EquivalenceOracle WpMethod where
    testSuite = wpmethodSuite
