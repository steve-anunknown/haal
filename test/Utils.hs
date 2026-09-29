{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use <$>" -}

module Utils (
    statesAreEquivalent,
    stateSpace,
    genState,
    NonMinimalMealy (..),
    Mealy (..),
    Input (..),
    Output (..),
    ArbWMethod (..),
    ArbWpMethod (..),
    ArbRandomWords (..),
    ArbRandomWalk (..),
    ArbRandomWMethod (..),
    ArbRandomWpMethod (..),
    OracleWrapper (..),
)
where

import qualified Data.Bifunctor as Bif
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Set as Set
import Haal.Automaton.MealyAutomaton (
    MealyAutomaton,
    mkMealyAutomaton,
    mealyTransitions,
 )
import Haal.BlackBox
import Haal.EquivalenceOracle.RandomWalk (
    RandomWalk,
    RandomWalkConfig (..),
    mkRandomWalk,
 )
import Haal.EquivalenceOracle.RandomWords (
    RandomWords,
    RandomWordsConfig (..),
    mkRandomWords,
 )
import Haal.EquivalenceOracle.WMethod (
    RandomWMethod,
    RandomWMethodConfig (..),
    WMethod,
    WMethodConfig (..),
    mkWMethod,
    mkRandomWMethod,
 )
import Haal.EquivalenceOracle.WpMethod (
    RandomWpMethod,
    RandomWpMethodConfig (..),
    WpMethod,
    WpMethodConfig (..),
    mkWpMethod,
    mkRandomWpMethod,
 )
import Haal.Experiment (EquivalenceOracle)
import System.Random
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, vectorOf)

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

newtype ArbRandomWpMethodConfig = ArbRandomWpMethodConfig RandomWpMethodConfig deriving (Show, Eq)
newtype ArbRandomWpMethod = ArbRandomWpMethod RandomWpMethod deriving (Show, Eq)

instance Arbitrary ArbWMethodConfig where
    arbitrary = do
        d <- choose (1, 5)
        return (ArbWMethodConfig (WMethodConfig d))
instance Arbitrary ArbWMethod where
    arbitrary = do
        (ArbWMethodConfig config) <- arbitrary :: Gen ArbWMethodConfig
        return (ArbWMethod (either error id (mkWMethod config)))

instance Arbitrary ArbWpMethodConfig where
    arbitrary = do
        d <- choose (1, 5)
        return (ArbWpMethodConfig (WpMethodConfig d))
instance Arbitrary ArbWpMethod where
    arbitrary = do
        (ArbWpMethodConfig config) <- arbitrary :: Gen ArbWpMethodConfig
        return (ArbWpMethod (either error id (mkWpMethod config)))

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
        return (ArbRandomWords (either error id (mkRandomWords config)))

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
        return (ArbRandomWalk (either error id (mkRandomWalk config)))

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
        return (ArbRandomWMethod (either error id (mkRandomWMethod config)))

instance Arbitrary ArbRandomWpMethodConfig where
    arbitrary = do
        seed <- choose (17, 69)
        let randGen = mkStdGen seed
        e <- choose (1, 10)
        m <- choose (1, e)
        l <- choose (1, 10000)
        return (ArbRandomWpMethodConfig (RandomWpMethodConfig randGen e m l))

instance Arbitrary ArbRandomWpMethod where
    arbitrary = do
        (ArbRandomWpMethodConfig config) <- arbitrary :: Gen ArbRandomWpMethodConfig
        return (ArbRandomWpMethod (either error id (mkRandomWpMethod config)))

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

instance OracleWrapper ArbRandomWpMethod RandomWpMethod where
    unwrap (ArbRandomWpMethod o) = o

newtype Mealy i o = Mealy (MealyAutomaton Int i o) deriving (Show)

instance
    ( Arbitrary i
    , Arbitrary o
    , FiniteOrd i
    , FiniteOrd o
    ) =>
    Arbitrary (Mealy i o)
    where
    arbitrary = do
        let sts = stateSpace
        delta <- generateDelta sts
        lambda <- generateLambda sts

        initialState <- elements sts
        currentState <- elements sts

        return
            ( Mealy
                ( update
                    (mkMealyAutomaton delta lambda (Set.fromList sts) initialState)
                    currentState
                )
            )
      where
        generateDelta :: [Int] -> Gen (Int -> i -> Int)
        generateDelta sts = do
            let
                ins = Set.toList $ inputs (undefined :: MealyAutomaton Int i o)
                complete = [(st, inp) | st <- sts, inp <- ins]
                (numS, numI) = Bif.bimap List.length List.length (sts, ins)
            matching <- vectorOf (numS * numI) (choose (0, numS - 1))
            let stateOutputs = [sts !! index | index <- matching]
                stateMappings = Map.fromList $ List.zip complete stateOutputs
            fallbackState <- elements sts
            return $ \s i -> Data.Maybe.fromMaybe fallbackState (Map.lookup (s, i) stateMappings)

        generateLambda :: [Int] -> Gen (Int -> i -> o)
        generateLambda sts = do
            let
                ins = Set.toList $ inputs (undefined :: MealyAutomaton Int i o)
                outs = Set.toList $ outputs (undefined :: MealyAutomaton Int i o)
                complete = [(st, inp) | st <- sts, inp <- ins]
                (numS, numI) = Bif.bimap List.length List.length (sts, ins)
                numO = List.length outs
            matching <- vectorOf (numS * numI) (choose (0, numO - 1))
            let outputOutputs = [outs !! index | index <- matching]
                outputMappings = Map.fromList $ List.zip complete outputOutputs
            fallbackOutput <- arbitrary :: Gen o
            return $ \s i -> Data.Maybe.fromMaybe fallbackOutput (Map.lookup (s, i) outputMappings)

data Input = A | B | C | D deriving (Show, Eq, Ord, Enum, Bounded)
data Output = X | Y | Z | W deriving (Show, Eq, Ord, Enum, Bounded)

{- | The states of the generated test automata. 'NonMinimalMealy' needs at
least 6-7 states, otherwise too many test cases are discarded.
-}
stateSpace :: [Int]
stateSpace = [0 .. 7]

-- | Generate a state that belongs to 'stateSpace'.
genState :: Gen Int
genState = elements stateSpace

-- Arbitrary instances for Input and Output
instance Arbitrary Input where
    arbitrary = elements [A, B, C, D]

instance Arbitrary Output where
    arbitrary = elements [X, Y, Z, W]

newtype NonMinimalMealy = NonMinimalMealy (MealyAutomaton Int Input Output) deriving (Show)

instance Arbitrary NonMinimalMealy where
    arbitrary = do
        let sts = stateSpace
        delta <- generateDelta sts
        lambda <- generateLambda sts

        initialState <- elements sts
        currentState <- elements sts

        return
            ( NonMinimalMealy
                ( update
                    (mkMealyAutomaton delta lambda (Set.fromList sts) initialState)
                    currentState
                )
            )
      where
        generateDelta :: [Int] -> Gen (Int -> Input -> Int)
        generateDelta sts = do
            let
                ins = Set.toList $ inputs (undefined :: MealyAutomaton Int Input Output)
                (numS, numI) = Bif.bimap List.length List.length (sts, ins)
                same = numS `div` 2
                nonMinimal = [(st, inp) | st <- take same sts, inp <- ins]
                rest = [(st, inp) | st <- drop same sts, inp <- ins]
            nonMinimalMatching1 <- vectorOf numI (choose (0, numS - 1))
            nonMinimalMatching2 <- vectorOf ((numS - same) * numI) (choose (0, numS - 1))
            let stateOutputs1 = [sts !! index | index <- concat (replicate same nonMinimalMatching1)]
                stateOutputs2 = [sts !! index | index <- nonMinimalMatching2]
                nonMinimalMappings = Map.fromList $ List.zip (nonMinimal ++ rest) (stateOutputs1 ++ stateOutputs2)
            fallbackState <- elements sts
            return $ \s i -> Data.Maybe.fromMaybe fallbackState (Map.lookup (s, i) nonMinimalMappings)

        generateLambda :: [Int] -> Gen (Int -> Input -> Output)
        generateLambda sts = do
            let
                ins = Set.toList $ inputs (undefined :: MealyAutomaton Int Input Output)
                outs = Set.toList $ outputs (undefined :: MealyAutomaton Int Input Output)
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
statesAreEquivalent :: MealyAutomaton Int Input Output -> Int -> Int -> Bool
statesAreEquivalent _ s1 s2 | s1 == s2 = True
statesAreEquivalent automaton s1 s2 =
    all (\i -> trans Map.! (s1, i) == trans Map.! (s2, i)) (inputs automaton)
  where
    trans = mealyTransitions automaton
