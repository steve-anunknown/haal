{- | Serialization, parsing, and code generation for Mealy automata in DOT
format, following the convention used by AALpy and LearnLib.
-}
module Haal.Dot (
    mealyToDot,
    ParsedMealy (..),
    parseDot,
    generateModule,
) where

import Data.Char (isAlphaNum, isDigit, isLower, isSpace, toUpper)
import Data.List (intercalate, isInfixOf, isPrefixOf, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

import Haal.Automaton.MealyAutomaton (MealyAutomaton, mealyTransitions)
import Haal.BlackBox (Automaton (..), FiniteOrd, initial)

-- ---------------------------------------------------------------------------
-- Serializer
-- ---------------------------------------------------------------------------

{- | Serialize a 'MealyAutomaton' to a DOT format string.

  The output follows the AALpy\/LearnLib convention:

  * States are rendered as @s0@, @s1@, … in ascending order, with their
    'show' representation as the node label.
  * Edge labels have the form @\"input\/output\"@.
  * The initial state is indicated by a @__start0@ dummy node.

  Returns @'Left' err@ if any input or output symbol's 'show' representation
  contains @\'\/\'@, as that would make the DOT file impossible to parse back
  correctly.
-}
mealyToDot ::
    (FiniteOrd s, FiniteOrd i, Show s, Show i, Show o) =>
    MealyAutomaton s i o ->
    Either String String
mealyToDot m = do
    let badInputs = filter ('/' `elem`) (map (show . snd) (Map.toList inputSet))
        badOutputs = filter ('/' `elem`) (map (show . snd . snd) (Map.toList trans))
    case (badInputs, badOutputs) of
        (i : _, _) -> Left ("Input label contains '/': " ++ i)
        (_, o : _) -> Left ("Output label contains '/': " ++ o)
        _ ->
            Right $
                unlines $
                    ["digraph haal {"]
                        ++ zipWith (curry nodeDecl) [0 :: Int ..] sortedStates
                        ++ map edgeDecl (Map.toList trans)
                        ++ [ "\t__start0 [label=\"\" shape=none];"
                           , "\t__start0 -> " ++ nodeId (initial m) ++ " [label=\"\"];"
                           , "}"
                           ]
  where
    sortedStates = Set.toAscList (states m)
    stateIndex = Map.fromList (zip sortedStates [0 :: Int ..])
    trans = mealyTransitions m
    inputSet = Map.fromList [((s, i), i) | (s, i) <- Map.keys trans]

    nodeId s = "s" ++ show (stateIndex Map.! s)

    nodeDecl (i, s) =
        "\ts" ++ show i ++ " [label=\"" ++ show s ++ "\"];"

    edgeDecl ((s, i), (s', o)) =
        "\t"
            ++ nodeId s
            ++ " -> "
            ++ nodeId s'
            ++ " [label=\""
            ++ show i
            ++ "/"
            ++ show o
            ++ "\"];"

-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

{- | The raw representation of a Mealy automaton parsed from DOT format.
  State, input, and output names are kept as 'String's.
  States are ordered with the initial state first, then others in order of
  first appearance in the transition list.
-}
data ParsedMealy = ParsedMealy
    { parsedInitState :: String
    , parsedStates :: [String]
    , parsedInputs :: [String]
    , parsedOutputs :: [String]
    , parsedTrans :: [(String, String, String, String)]
    -- ^ @(srcState, input, dstState, output)@
    , parsedWarnings :: [String]
    }
    deriving (Show)

{- | Parse a DOT format string representing a Mealy automaton.

  Supports the output format of both AALpy and LearnLib:

  * Initial state indicated by @__start0 -> \<state\>@
  * Edge labels of the form @\"input\/output\"@ or @\"input \/ output\"@
    (whitespace around @\/@ is stripped)

  Returns @'Left' err@ with a descriptive message on failure.
-}
parseDot :: String -> Either String ParsedMealy
parseDot src = do
    let ls = lines src
    initSt <- findInit ls
    trans <- collectTrans ls
    let allStates = nub $ concatMap (\(s, _, d, _) -> [s, d]) trans
    if initSt `notElem` allStates
        then Left ("Initial state '" ++ initSt ++ "' does not appear in any transition")
        else do
            let otherStates = filter (/= initSt) allStates
                stateOrder = initSt : otherStates
                inputSyms = nub $ map (\(_, i, _, _) -> i) trans
                outputSyms = nub $ map (\(_, _, _, o) -> o) trans
                warnings = slashWarnings inputSyms outputSyms
            return
                ParsedMealy
                    { parsedInitState = initSt
                    , parsedStates = stateOrder
                    , parsedInputs = inputSyms
                    , parsedOutputs = outputSyms
                    , parsedTrans = trans
                    , parsedWarnings = warnings
                    }

-- ---------------------------------------------------------------------------
-- Code generator
-- ---------------------------------------------------------------------------

{- | Generate a Haskell module from a 'ParsedMealy'.

  @generateModule modName valName pm@ produces source for a module named
  @modName@ containing:

  * A @data \<modName\>Input@ type whose constructors are the sanitized input
    symbols, deriving @Show, Eq, Ord, Enum, Bounded@.
  * A @data \<modName\>Output@ type, similarly for output symbols.
  * A value @valName :: MealyAutomaton Int \<modName\>Input \<modName\>Output@.

  Returns @'Left' err@ if two distinct symbols sanitize to the same
  constructor name.
-}
generateModule :: String -> String -> ParsedMealy -> Either String String
generateModule modName valName pm = do
    inputCons <- sanitizeAll "In_" "input" (parsedInputs pm)
    outputCons <- sanitizeAll "Out_" "output" (parsedOutputs pm)
    let stateNames = parsedStates pm
        n = length stateNames
        stateIdx = Map.fromList (zip stateNames [0 :: Int ..])
        inputConMap = Map.fromList (zip (parsedInputs pm) inputCons)
        outputConMap = Map.fromList (zip (parsedOutputs pm) outputCons)
        inputType = modName ++ "Input"
        outputType = modName ++ "Output"
        deltaLines = map (mkDeltaLine stateIdx inputConMap) (parsedTrans pm)
        lambdaLines = map (mkLambdaLine stateIdx inputConMap outputConMap) (parsedTrans pm)
    return $
        unlines $
            [ "-- Generated by haal-gen. Do not edit manually."
            , "module " ++ modName
            , "    ( " ++ inputType ++ " (..)"
            , "    , " ++ outputType ++ " (..)"
            , "    , " ++ valName
            , "    ) where"
            , ""
            , "import qualified Data.Set as Set"
            , "import Haal.Automaton.MealyAutomaton (MealyAutomaton, mkMealyAutomaton)"
            , ""
            , "data " ++ inputType
            ]
                ++ enumDecl inputCons
                ++ [ ""
                   , "data " ++ outputType
                   ]
                ++ enumDecl outputCons
                ++ [ ""
                   , valName ++ " :: MealyAutomaton Int " ++ inputType ++ " " ++ outputType
                   , valName
                        ++ " = mkMealyAutomaton delta lambda (Set.fromList [0.."
                        ++ show (n - 1)
                        ++ "]) 0"
                   , "  where"
                   ]
                ++ map ("    " ++) deltaLines
                ++ ["    delta _ _ = error \"haal-gen: undefined transition\""]
                ++ map ("    " ++) lambdaLines
                ++ ["    lambda _ _ = error \"haal-gen: undefined transition\""]

-- ---------------------------------------------------------------------------
-- Code generation helpers
-- ---------------------------------------------------------------------------

enumDecl :: [String] -> [String]
enumDecl [] = ["    deriving (Show, Eq, Ord, Enum, Bounded)"]
enumDecl (c : cs) =
    ["    = " ++ c]
        ++ map ("    | " ++) cs
        ++ ["    deriving (Show, Eq, Ord, Enum, Bounded)"]

mkDeltaLine ::
    Map.Map String Int ->
    Map.Map String String ->
    (String, String, String, String) ->
    String
mkDeltaLine stateIdx inputConMap (src, inp, dst, _) =
    "delta " ++ show si ++ " " ++ ic ++ " = " ++ show di
  where
    si = stateIdx Map.! src
    di = stateIdx Map.! dst
    ic = inputConMap Map.! inp

mkLambdaLine ::
    Map.Map String Int ->
    Map.Map String String ->
    Map.Map String String ->
    (String, String, String, String) ->
    String
mkLambdaLine stateIdx inputConMap outputConMap (src, inp, _, out) =
    "lambda " ++ show si ++ " " ++ ic ++ " = " ++ oc
  where
    si = stateIdx Map.! src
    ic = inputConMap Map.! inp
    oc = outputConMap Map.! out

{- | Sanitize a list of symbols to valid Haskell constructor names using the
  given prefix, failing if two distinct symbols would produce the same name.
-}
sanitizeAll :: String -> String -> [String] -> Either String [String]
sanitizeAll prefix kind syms =
    let sanitized = map (sanitizeName prefix) syms
        byConName = Map.fromListWith (++) (zip sanitized (map (: []) syms))
        collisions =
            [ (con, originals)
            | (con, originals) <- Map.toList byConName
            , length originals > 1
            ]
     in case collisions of
            [] -> Right sanitized
            cs ->
                Left $
                    "Colliding "
                        ++ kind
                        ++ " constructor names:\n"
                        ++ unlines
                            [ "  "
                                ++ intercalate ", " (map show originals)
                                ++ " → "
                                ++ con
                            | (con, originals) <- cs
                            ]

{- | Sanitize a symbol string to a valid Haskell constructor name:
  replace non-alphanumeric characters with @_@, strip leading\/trailing
  underscores, capitalise the first character, prefix with @N@ if it
  starts with a digit, and prepend the given prefix.
-}
sanitizeName :: String -> String -> String
sanitizeName prefix s = prefix ++ base
  where
    s1 = map (\c -> if isAlphaNum c then c else '_') s
    s2 = reverse (dropWhile (== '_') (reverse (dropWhile (== '_') s1)))
    s3 = if null s2 then "Unknown" else s2
    base = case s3 of
        (c : cs)
            | isLower c -> toUpper c : cs
            | isDigit c -> 'N' : s3
            | otherwise -> s3
        [] -> "Unknown"

-- ---------------------------------------------------------------------------
-- Parser helpers
-- ---------------------------------------------------------------------------

slashWarnings :: [String] -> [String] -> [String]
slashWarnings inputs outputs =
    [ "Input symbol contains '/': \"" ++ s ++ "\" — label may have been misparsed"
    | s <- inputs
    , '/' `elem` s
    ]
        ++ [ "Output symbol contains '/': \"" ++ s ++ "\" — label may have been misparsed"
           | s <- outputs
           , '/' `elem` s
           ]

findInit :: [String] -> Either String String
findInit ls =
    case mapMaybe extractInit ls of
        [] -> Left "No initial state marker found (expected '__start0 -> <state>')"
        (s : _) -> Right s
  where
    extractInit l
        | "__start0" `isInfixOf` l && "->" `isInfixOf` l =
            let afterArrow = trim $ drop 2 $ snd $ breakOn "->" l
                name = takeWhile isStateChar afterArrow
             in if null name then Nothing else Just name
        | otherwise = Nothing

collectTrans :: [String] -> Either String [(String, String, String, String)]
collectTrans ls = concat <$> mapM process relevantLines
  where
    relevantLines = filter isEdgeLine ls
    isEdgeLine l = "->" `isInfixOf` l && not ("__start0" `isInfixOf` l)
    process l =
        case extractLabel l of
            Nothing -> Right []
            Just "" -> Right []
            Just lbl -> case splitLabel lbl of
                Left err -> Left ("Malformed label \"" ++ lbl ++ "\": " ++ err)
                Right (inp, out) ->
                    case parseEndpoints l of
                        Nothing -> Left ("Could not parse endpoints in: " ++ trim l)
                        Just (src, dst) -> Right [(src, inp, dst, out)]

extractLabel :: String -> Maybe String
extractLabel l =
    case findSubstr "label=" l of
        Nothing -> Nothing
        Just after ->
            case dropWhile isSpace after of
                '"' : rest -> Just $ takeWhile (/= '"') rest
                rest -> Just $ takeWhile (\c -> c /= ',' && c /= ']' && not (isSpace c)) rest

parseEndpoints :: String -> Maybe (String, String)
parseEndpoints l =
    let (srcPart, rest) = breakOn "->" l
        src = trim srcPart
        afterArrow = trim (drop 2 rest)
        dst = trim $ takeWhile (\c -> c /= '[' && c /= ';') afterArrow
     in if null src || null dst then Nothing else Just (src, dst)

splitLabel :: String -> Either String (String, String)
splitLabel lbl =
    case breakOn "/" lbl of
        (_, []) -> Left "missing '/' separator"
        (inp, _ : out) ->
            let i = trim inp
                o = trim out
             in if null i
                    then Left "empty input"
                    else Right (i, o)

findSubstr :: String -> String -> Maybe String
findSubstr _ [] = Nothing
findSubstr needle haystack@(_ : xs)
    | needle `isPrefixOf` haystack = Just (drop (length needle) haystack)
    | otherwise = findSubstr needle xs

breakOn :: String -> String -> (String, String)
breakOn _ [] = ([], [])
breakOn needle haystack@(x : xs)
    | needle `isPrefixOf` haystack = ([], haystack)
    | otherwise =
        let (pre, rest) = breakOn needle xs
         in (x : pre, rest)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

isStateChar :: Char -> Bool
isStateChar c = isAlphaNum c || c == '_'
