-- | This module tests the DOT parser and the table encoding used by haal-gen.
module DotSpec (
    spec,
)
where

import Data.Char (chr, ord)
import Data.Either (isLeft)
import Data.List (isInfixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Haal.Automaton.MealyAutomaton (
    MealyAutomaton,
    mealyTransitions,
    mkMealyAutomatonTable,
 )
import Haal.BlackBox (initial, states)
import Haal.Dot
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (Property, counterexample, property, (===))
import Utils (Input, Mealy (..), Output)

-- A small automaton whose states and symbols appear out of alphabetical order.
smallDot :: String
smallDot =
    unlines
        [ "digraph g {"
        , "\t__start0 [label=\"\" shape=none];"
        , "\ts0 [label=\"s0\"];"
        , "\ts0 -> s2 [label=\"b/x\"];"
        , "\ts0 -> s1 [label=\"a/y\"];"
        , "\ts1 -> s1 [label=\"b/x\"];"
        , "\ts1 -> s0 [label=\"a/x\"];"
        , "\ts2 -> s2 [label=\"a/y\"];"
        , "\ts2 -> s0 [label=\"b/y\"];"
        , "\t__start0 -> s0;"
        , "}"
        ]

-- 'smallDot' with the line of one transition replaced.
smallDotWith :: String -> String -> String
smallDotWith old new = unlines [if l == old then new else l | l <- lines smallDot]

-- Encode an automaton with states 0 .. n - 1 as tables, without 'Haal.Dot'.
-- 'mealyTransitions' is ordered by (state, input), which is row-major order.
encode :: MealyAutomaton Int Input Output -> (Int, String, String)
encode m = (Set.size (states m), map (chr . fst) entries, map (chr . fromEnum . snd) entries)
  where
    entries = Map.elems (mealyTransitions m)

prop_tableRoundTrip :: Mealy Input Output -> Property
prop_tableRoundTrip (Mealy m) =
    let (n, deltaTable, lambdaTable) = encode m
     in mkMealyAutomatonTable n (initial m) deltaTable lambdaTable === Right m

-- Every transition of a serialized automaton is reproduced by its table.
prop_mealyTableMatchesParsed :: Mealy Input Output -> Property
prop_mealyTableMatchesParsed (Mealy m) =
    case mealyToDot m >>= parseDot of
        Left err -> counterexample err False
        Right pm -> case mealyTable pm of
            Left err -> counterexample err False
            Right t ->
                let index names = Map.fromList (zip names [0 :: Int ..])
                    stateIdx = index (parsedStates pm)
                    inputIdx = index (parsedInputs pm)
                    outputIdx = index (parsedOutputs pm)
                    n = length (parsedStates pm)
                    k = length (parsedInputs pm)
                    keys = [(s, i) | s <- [0 .. n - 1], i <- [0 .. k - 1]]
                    decoded =
                        Map.fromList (zip keys (zip (map ord (tableDelta t)) (map ord (tableLambda t))))
                    expected =
                        Map.fromList
                            [ ((stateIdx Map.! src, inputIdx Map.! inp), (stateIdx Map.! dst, outputIdx Map.! out))
                            | (src, inp, dst, out) <- parsedTrans pm
                            ]
                 in (tableStates t, length (tableDelta t), length (tableLambda t), decoded)
                        === (n, n * k, n * k, expected)

spec :: Spec
spec = do
    describe "MealyAutomaton.mkMealyAutomatonTable" $ do
        it "rebuilds an automaton from its tables" $
            property prop_tableRoundTrip
        it "rejects tables of the wrong length" $
            (mkMealyAutomatonTable 1 0 "\0\0\0" "\0\0\0\0" :: Either String (MealyAutomaton Int Input Output))
                `shouldSatisfy` isLeft
        it "rejects transitions to states that do not exist" $
            (mkMealyAutomatonTable 1 0 "\0\0\0\1" "\0\0\0\0" :: Either String (MealyAutomaton Int Input Output))
                `shouldSatisfy` isLeft
        it "rejects outputs that do not exist" $
            (mkMealyAutomatonTable 1 0 "\0\0\0\0" "\0\0\0\4" :: Either String (MealyAutomaton Int Input Output))
                `shouldSatisfy` isLeft
        it "rejects an initial state that does not exist" $
            (mkMealyAutomatonTable 1 1 "\0\0\0\0" "\0\0\0\0" :: Either String (MealyAutomaton Int Input Output))
                `shouldSatisfy` isLeft
        it "rejects an automaton without states" $
            (mkMealyAutomatonTable 0 0 "" "" :: Either String (MealyAutomaton Int Input Output))
                `shouldSatisfy` isLeft

    describe "Dot.parseDot" $
        it "keeps states and symbols in order of first appearance" $
            fmap (\pm -> (parsedStates pm, parsedInputs pm, parsedOutputs pm)) (parseDot smallDot)
                `shouldBe` Right (["s0", "s2", "s1"], ["b", "a"], ["x", "y"])

    describe "Dot.mealyTable" $ do
        it "encodes a small automaton in row-major order" $
            (parseDot smallDot >>= mealyTable)
                `shouldBe` Right (MealyTable 3 "\1\2\0\1\2\0" "\0\1\1\1\0\0")
        it "reproduces every transition of a serialized automaton" $
            property prop_mealyTableMatchesParsed
        it "rejects an automaton with a missing transition" $
            (parseDot (smallDotWith "\ts2 -> s0 [label=\"b/y\"];" "") >>= mealyTable)
                `shouldSatisfy` either ("Incomplete" `isInfixOf`) (const False)
        it "rejects an automaton with conflicting transitions" $
            (parseDot (smallDot ++ "\ts2 -> s1 [label=\"b/y\"];\n") >>= mealyTable)
                `shouldSatisfy` either ("Nondeterministic" `isInfixOf`) (const False)
        it "accepts a duplicated identical transition" $
            (parseDot (smallDot ++ "\ts2 -> s0 [label=\"b/y\"];\n") >>= mealyTable)
                `shouldBe` (parseDot smallDot >>= mealyTable)

    describe "Dot.generateModule" $ do
        it "builds the automaton from tables" $
            (parseDot smallDot >>= generateModule "Small" "small")
                `shouldSatisfy` either (const False) ("mkMealyAutomatonTable 3 0 deltaTable lambdaTable" `isInfixOf`)
        it "rejects an incomplete automaton" $
            (parseDot (smallDotWith "\ts2 -> s0 [label=\"b/y\"];" "") >>= generateModule "Small" "small")
                `shouldSatisfy` isLeft
