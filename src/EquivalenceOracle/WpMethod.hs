{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements the WpMethod.
module EquivalenceOracle.WpMethod (
    WpMethod (..),
    WpMethodConfig (..),
    wpmethodSuiteSize,
) where

import BlackBox
import Control.Monad (replicateM)
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
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
    , Hashable s
    , Hashable i
    ) =>
    WpMethod ->
    aut i o ->
    (WpMethod, [[i]])
wpmethodSuite wpm@(WpMethod (WpMethodConfig{wpDepth = d})) aut = (wpm, suite)
  where
    alphabet = inputs aut
    stateCover = accessSequences aut
    localSuf =
        HMS.fromList
            [ (st, localCharacterizingSet aut st) | st <- HS.toList $ states aut
            ]
    globalSuf = globalCharacterizingSet aut

    transitionCover =
        [ acc ++ [a]
        | acc <- HMS.elems stateCover
        , a <- HS.toList alphabet
        ]
    difference =
        HS.fromList (HMS.elems stateCover)
            `HS.difference` HS.fromList transitionCover

    firstPhase =
        concat
            [ [ acc ++ middle ++ suf
              | acc <- HMS.elems stateCover
              , suf <- HS.toList globalSuf
              ]
            | fixed <- [0 .. d]
            , middle <- replicateM fixed $ HS.toList alphabet
            ]

    secondPhase =
        concat
            [ [ acc ++ middle ++ suf
              | acc <- HS.toList difference
              , suf <- HS.toList $ localSuf HMS.! current (fst (walk aut (acc ++ middle)))
              ]
            | fixed <- [0 .. d]
            , middle <- replicateM fixed $ HS.toList alphabet
            ]

    suite = firstPhase ++ secondPhase

instance EquivalenceOracle WpMethod where
    testSuite = wpmethodSuite
