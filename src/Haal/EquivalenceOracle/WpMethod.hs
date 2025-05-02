{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements the WpMethod.
module Haal.EquivalenceOracle.WpMethod (
    WpMethod (..),
    WpMethodConfig (..),
    RandomWpMethod (..),
    RandomWpMethodConfig (..),
    wpmethodSuiteSize,
    randomWpMethodSuite,
) where

import Control.Monad (replicateM)
import Control.Monad.State (MonadState (state), State, runState)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Haal.BlackBox
import Haal.Experiment
import System.Random (Random (randomR), StdGen)

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
    , FiniteOrd i
    , FiniteOrd s
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
        Map.fromAscList
            [ (st, localCharacterizingSet aut st) | st <- Set.toAscList $ states aut
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

data RandomWpMethodConfig = RandomWpMethodConfig
    { rwpGen :: StdGen
    -- ^ Random generator.
    , rwpExpected :: Int
    -- ^ Expected depth of random walk.
    , rwpMin :: Int
    -- ^ Minimum depth of random walk.
    , rwpLimit :: Int
    -- ^ Maximum number of queries.
    }
    deriving (Show, Eq)

newtype RandomWpMethod = RandomWpMethod RandomWpMethodConfig deriving (Show, Eq)

randomWpMethodSuite ::
    forall aut i o s.
    ( Automaton aut s
    , FiniteOrd i
    , FiniteOrd s
    , Eq o
    ) =>
    RandomWpMethod ->
    aut i o ->
    (RandomWpMethod, [[i]])
randomWpMethodSuite
    ( RandomWpMethod
            conf@RandomWpMethodConfig
                { rwpGen = g
                , rwpExpected = e
                , rwpMin = mi
                , rwpLimit = lim
                }
        )
    aut = (RandomWpMethod (conf{rwpGen = genfinal}), suite)
      where
        alphabet = inputs aut
        prefixes = accessSequences aut
        localSuf =
            Map.fromAscList
                [ (st, localCharacterizingSet aut st) | st <- Set.toAscList $ states aut
                ]
        globalSuf = globalCharacterizingSet aut

        (suite, genfinal) = runState (replicateM lim genTestCase) g

        genTestCase :: State StdGen [i]
        genTestCase = do
            prefixIdx <- state $ randomR (0, Map.size prefixes - 1)
            let (_, prefix) = prefixIdx `Map.elemAt` prefixes
            middle <- genExpectedLength
            local <- state $ randomR (False, True)
            if local
                then do
                    let curr = current $ fst (walk aut (prefix ++ middle))
                        suffixSet = localSuf Map.! curr
                    suffixIdx <- state $ randomR (0, Set.size suffixSet - 1)
                    let suffix = suffixIdx `Set.elemAt` suffixSet
                    return $ prefix ++ middle ++ suffix
                else do
                    globalIdx <- state $ randomR (0, Set.size globalSuf - 1)
                    let suffix = globalIdx `Set.elemAt` globalSuf
                    return $ prefix ++ middle ++ suffix

        genExpectedLength :: State StdGen [i]
        genExpectedLength = state $ go [] mi
          where
            go :: [i] -> Int -> StdGen -> ([i], StdGen)
            go acc minim gen =
                let (continue, gen') = randomR (0.0 :: Double, 1.0) gen
                 in if minim > 0 || continue > 1 / (fromIntegral e + 1)
                        then
                            let (idx, gen'') = randomR (0, Set.size alphabet - 1) gen'
                                nextChar = idx `Set.elemAt` alphabet
                             in go (nextChar : acc) (minim - 1) gen''
                        else (acc, gen)

instance EquivalenceOracle WpMethod where
    testSuite = wpmethodSuite

instance EquivalenceOracle RandomWpMethod where
    testSuite = randomWpMethodSuite
