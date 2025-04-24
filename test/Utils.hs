{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils (
    findReachable,
    statesAreEquivalent,
    NonMinimalMealy (..),
    Mealy (..),
    Input (..),
    Output (..),
    State (..),
    ArbWMethod (..),
    ArbWpMethod (..),
    ArbRandomWords (..),
    ArbRandomWalk (..),
    ArbRandomWMethod (..),
    OracleWrapper (..),
)
where

import Automaton.MealyAutomaton (
    MealyAutomaton (..),
    mealyDelta,
    mealyLambda,
 )

import BlackBox
import qualified Data.Bifunctor as Bif
import Data.Hashable (Hashable)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe
import GHC.Generics
import System.Random
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, vectorOf)

import qualified Data.Set as Set
import qualified Data.HashSet as HS
import EquivalenceOracle.RandomWalk (
    RandomWalk (RandomWalk),
    RandomWalkConfig (RandomWalkConfig),
 )
import EquivalenceOracle.RandomWords (
    RandomWords (RandomWords),
    RandomWordsConfig (RandomWordsConfig),
 )
import EquivalenceOracle.WMethod (
    RandomWMethod (RandomWMethod),
    RandomWMethodConfig (RandomWMethodConfig),
    WMethod (WMethod),
    WMethodConfig (WMethodConfig),
 )
import EquivalenceOracle.WpMethod (
    WpMethod (WpMethod),
    WpMethodConfig (WpMethodConfig),
 )
import Experiment (EquivalenceOracle)

newtype ArbWMethodConfig = ArbWMethodConfig WMethodConfig deriving (Show, Eq)
newtype ArbWMethod = ArbWMethod WMethod deriving (Show, Eq)

newtype ArbWpMethodConfig = ArbWpMethodConfig WpMethodConfig deriving (Show, Eq)
newtype ArbWpMethod = ArbWpMethod WpMethod deriving (Show, Eq)

newtype ArbRandomWordsConfig = ArbRandomWordsConfig RandomWordsConfig deriving (Show, Eq)
newtype ArbRandomWords = ArbRandomWords RandomWords deriving (Show, Eq)

newtype ArbRandomWalkConfig = ArbRandomWalkConfig RandomWalkConfig deriving (Show, Eq)
newtype ArbRandomWalk = ArbRandomWalk RandomWalk deriving (Show, Eq)

newtype ArbRandomWMethodConfig = ArbRandomWMethodConfig RandomWMethodConfig deriving (Show, Eq)
newtype ArbRandomWMethod = ArbRandomWMethod RandomWMethod deriving (Show, Eq)

instance Arbitrary ArbWMethodConfig where
    arbitrary = do
        d <- choose (1, 5)
        return (ArbWMethodConfig (WMethodConfig d))
instance Arbitrary ArbWMethod where
    arbitrary = do
        (ArbWMethodConfig config) <- arbitrary :: Gen ArbWMethodConfig
        return (ArbWMethod (WMethod config))

instance Arbitrary ArbWpMethodConfig where
    arbitrary = do
        d <- choose (1, 5)
        return (ArbWpMethodConfig (WpMethodConfig d))
instance Arbitrary ArbWpMethod where
    arbitrary = do
        (ArbWpMethodConfig config) <- arbitrary :: Gen ArbWpMethodConfig
        return (ArbWpMethod (WpMethod config))

instance Arbitrary ArbRandomWordsConfig where
    arbitrary = do
        lim <- choose (100, 10000)
        minL <- choose (1, 10)
        maxL <- choose (minL, 11)
        seed <- choose (17, 69)
        let randGen = mkStdGen seed
        return (ArbRandomWordsConfig (RandomWordsConfig randGen lim minL maxL))
instance Arbitrary ArbRandomWords where
    arbitrary = do
        (ArbRandomWordsConfig config) <- arbitrary :: Gen ArbRandomWordsConfig
        return (ArbRandomWords (RandomWords config))

instance Arbitrary ArbRandomWalkConfig where
    arbitrary = do
        lim <- choose (100, 10000)
        restart <- choose (0.0, 1.0)
        seed <- choose (17, 69)
        let randGen = mkStdGen seed
        return (ArbRandomWalkConfig (RandomWalkConfig randGen lim restart))
instance Arbitrary ArbRandomWalk where
    arbitrary = do
        (ArbRandomWalkConfig config) <- arbitrary :: Gen ArbRandomWalkConfig
        return (ArbRandomWalk (RandomWalk config))

instance Arbitrary ArbRandomWMethodConfig where
    arbitrary = do
        seed <- choose (17, 69)
        let randGen = mkStdGen seed
        wpr <- choose (10, 20)
        wl <- choose (1, 5)
        return (ArbRandomWMethodConfig (RandomWMethodConfig randGen wpr wl))
instance Arbitrary ArbRandomWMethod where
    arbitrary = do
        (ArbRandomWMethodConfig config) <- arbitrary :: Gen ArbRandomWMethodConfig
        return (ArbRandomWMethod (RandomWMethod config))

class (EquivalenceOracle oracle) => OracleWrapper w oracle | w -> oracle where
    unwrap :: w -> oracle

instance OracleWrapper ArbWMethod WMethod where
    unwrap (ArbWMethod o) = o

instance OracleWrapper ArbWpMethod WpMethod where
    unwrap (ArbWpMethod o) = o

instance OracleWrapper ArbRandomWords RandomWords where
    unwrap (ArbRandomWords o) = o

instance OracleWrapper ArbRandomWalk RandomWalk where
    unwrap (ArbRandomWalk o) = o

instance OracleWrapper ArbRandomWMethod RandomWMethod where
    unwrap (ArbRandomWMethod o) = o

newtype Mealy s i o = Mealy (MealyAutomaton s i o) deriving (Show)

instance
    ( Arbitrary i
    , Arbitrary o
    , Arbitrary s
    , Ord i
    , Enum i
    , Bounded i
    , Hashable i
    , Ord o
    , Enum o
    , Bounded o
    , Hashable o
    , Ord s
    , Enum s
    , Bounded s
    , Hashable s
    ) =>
    Arbitrary (Mealy s i o)
    where
    arbitrary = do
        let sts = [minBound .. maxBound]
        delta <- generateDelta sts
        lambda <- generateLambda sts

        initialState <- arbitrary
        currentState <- arbitrary

        return
            ( Mealy
                ( MealyAutomaton
                    { mealyDelta = delta
                    , mealyLambda = lambda
                    , mealyInitialS = initialState
                    , mealyCurrentS = currentState
                    , mealyStates = HS.fromList sts
                    }
                )
            )
      where
        generateDelta :: [s] -> Gen (s -> i -> s)
        generateDelta sts = do
            let
                ins = HS.toList $ inputs (undefined :: MealyAutomaton s i o)
                complete = [(st, inp) | st <- sts, inp <- ins]
                (numS, numI) = Bif.bimap List.length List.length (sts, ins)
            matching <- vectorOf (numS * numI) (choose (0, numS - 1))
            let stateOutputs = [sts !! index | index <- matching]
                stateMappings = Map.fromList $ List.zip complete stateOutputs
            fallbackState <- arbitrary :: Gen s
            return $ \s i -> Data.Maybe.fromMaybe fallbackState (Map.lookup (s, i) stateMappings)

        generateLambda :: [s] -> Gen (s -> i -> o)
        generateLambda sts = do
            let
                ins = HS.toList $ inputs (undefined :: MealyAutomaton s i o)
                outs = HS.toList $ outputs (undefined :: MealyAutomaton s i o)
                complete = [(st, inp) | st <- sts, inp <- ins]
                (numS, numI) = Bif.bimap List.length List.length (sts, ins)
                numO = List.length outs
            matching <- vectorOf (numS * numI) (choose (0, numO - 1))
            let outputOutputs = [outs !! index | index <- matching]
                outputMappings = Map.fromList $ List.zip complete outputOutputs
            fallbackOutput <- arbitrary :: Gen o
            return $ \s i -> Data.Maybe.fromMaybe fallbackOutput (Map.lookup (s, i) outputMappings)

data Input = A | B | C | D
    deriving stock (Show, Eq, Ord, Enum, Bounded)
    deriving anyclass (Hashable)
    deriving stock (Generic)

data Output = X | Y | Z | W
    deriving stock (Show, Eq, Ord, Enum, Bounded)
    deriving anyclass (Hashable)
    deriving stock (Generic)

data State = S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7
    deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
    deriving anyclass (Hashable)

-- Arbitrary instances for Input, Output, and State
instance Arbitrary Input where
    arbitrary = elements [A, B, C, D]

instance Arbitrary Output where
    arbitrary = elements [X, Y, Z, W]

instance Arbitrary State where
    arbitrary = elements [S0, S1, S2, S3, S4, S5, S6, S7]

newtype NonMinimalMealy = NonMinimalMealy (MealyAutomaton State Input Output) deriving (Show)

instance Arbitrary NonMinimalMealy where
    arbitrary = do
        let sts = [minBound .. maxBound]
        delta <- generateDelta sts
        lambda <- generateLambda sts

        initialState <- arbitrary :: Gen State
        currentState <- arbitrary :: Gen State

        return
            ( NonMinimalMealy
                ( MealyAutomaton
                    { mealyDelta = delta
                    , mealyLambda = lambda
                    , mealyInitialS = initialState
                    , mealyCurrentS = currentState
                    , mealyStates = HS.fromList sts
                    }
                )
            )
      where
        generateDelta :: [State] -> Gen (State -> Input -> State)
        generateDelta sts = do
            let
                ins = HS.toList $ inputs (undefined :: MealyAutomaton State Input Output)
                (numS, numI) = Bif.bimap List.length List.length (sts, ins)
                same = numS `div` 2
                nonMinimal = [(st, inp) | st <- take same sts, inp <- ins]
                rest = [(st, inp) | st <- drop same sts, inp <- ins]
            nonMinimalMatching1 <- vectorOf numI (choose (0, numS - 1))
            nonMinimalMatching2 <- vectorOf ((numS - same) * numI) (choose (0, numS - 1))
            let stateOutputs1 = [sts !! index | index <- concat (replicate same nonMinimalMatching1)]
                stateOutputs2 = [sts !! index | index <- nonMinimalMatching2]
                nonMinimalMappings = Map.fromList $ List.zip (nonMinimal ++ rest) (stateOutputs1 ++ stateOutputs2)
            fallbackState <- arbitrary :: Gen State
            return $ \s i -> Data.Maybe.fromMaybe fallbackState (Map.lookup (s, i) nonMinimalMappings)

        generateLambda :: [State] -> Gen (State -> Input -> Output)
        generateLambda sts = do
            let
                ins = HS.toList $ inputs (undefined :: MealyAutomaton State Input Output)
                outs = HS.toList $ outputs (undefined :: MealyAutomaton State Input Output)
                same = numS `div` 2
                nonMinimal = [(st, inp) | st <- take same sts, inp <- ins]
                rest = [(st, inp) | st <- drop same sts, inp <- ins]
                (numS, numI) = Bif.bimap List.length List.length (sts, ins)
                numO = List.length outs
            nonMinimalMatching1 <- vectorOf numI (choose (0, numO - 1))
            nonMinimalMatching2 <- vectorOf ((numS - same) * numI) (choose (0, numO - 1))
            let outputOutputs1 = [outs !! index | index <- concat (replicate same nonMinimalMatching1)]
                outputOutputs2 = [outs !! index | index <- nonMinimalMatching2]
                outputMappings = Map.fromList $ List.zip (nonMinimal ++ rest) (outputOutputs1 ++ outputOutputs2)
            fallbackOutput <- arbitrary :: Gen Output
            return $ \s i -> Data.Maybe.fromMaybe fallbackOutput (Map.lookup (s, i) outputMappings)

-- Two states are equivalent if their delta and lambda functions are equivalent.
statesAreEquivalent :: MealyAutomaton State Input Output -> State -> State -> Bool
statesAreEquivalent _ s1 s2 | s1 == s2 = True
statesAreEquivalent automaton s1 s2 =
    all (\i -> (delta s1 i, lambda s1 i) == (delta s2 i, lambda s2 i)) alphabet
  where
    delta = mealyDelta automaton
    lambda = mealyLambda automaton
    alphabet = inputs automaton

-- Starts a bfs from the initial state and finds all reachable states
-- findReachable :: MealyAutomaton State Input Output -> Set.Set State
-- findReachable automaton =
--     let initialState = mealyInitialS automaton
--         alphabet = inputs automaton
--         bfs visited [] = visited
--         bfs visited (curr : queue) =
--             let mo = update automaton curr
--                 nextStates = Set.map (current . fst . step mo) alphabet
--                 newVisited = Set.foldr Set.insert visited nextStates
--                 newQueue = queue ++ [s | s <- HS.toList nextStates, s `notElem` visited]
--              in bfs newVisited newQueue
--      in bfs (Set.singleton initialState) [initialState]

findReachable :: MealyAutomaton State Input Output -> HS.HashSet State
findReachable automaton =
    let initialState = mealyInitialS automaton
        alphabet = inputs automaton
        bfs visited queue =
            case queue of
                [] -> visited
                (curr : queue') ->
                    let mo = update automaton curr
                        nextStates = HS.map (current . fst . step mo) alphabet
                        newVisited = HS.union visited nextStates
                        -- Efficient queue management with a Set for fast membership checking
                        newQueue = HS.toList (HS.difference nextStates visited) ++ queue'
                     in bfs newVisited newQueue
     in bfs (HS.singleton initialState) [initialState]
