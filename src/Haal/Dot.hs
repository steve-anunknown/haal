{- | Serialization, parsing, and code generation for Mealy automata in DOT
format, following the convention used by AALpy and LearnLib.
-}
module Haal.Dot (
    mealyToDot,
    ParsedMealy (..),
    parseDot,
    MealyTable (..),
    mealyTable,
    generateModule,
) where

import Data.Char (chr, isAlphaNum, isDigit, isLower, isSpace, ord, toUpper)
import Data.List (intercalate, isInfixOf, isPrefixOf)
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
    let allStateSet = Set.fromList $ concatMap (\(s, _, d, _) -> [s, d]) trans
    if initSt `Set.notMember` allStateSet
        then Left ("Initial state '" ++ initSt ++ "' does not appear in any transition")
        else do
            let stateOrder = ordNub (initSt : concatMap (\(s, _, d, _) -> [s, d]) trans)
                inputSyms = ordNub $ map (\(_, i, _, _) -> i) trans
                outputSyms = ordNub $ map (\(_, _, _, o) -> o) trans
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
-- Transition tables
-- ---------------------------------------------------------------------------

{- | A complete, deterministic Mealy automaton in the table encoding expected
  by 'Haal.Automaton.MealyAutomaton.mkMealyAutomatonTable'.

  States, inputs, and outputs are numbered by their position in
  'parsedStates', 'parsedInputs', and 'parsedOutputs', so the initial state
  is always @0@. The entry at position @s * k + j@ of each table, where @k@
  is the number of inputs, describes state @s@ on input @j@, encoded as the
  'Char' with that code point.
-}
data MealyTable = MealyTable
    { tableStates :: Int
    -- ^ The number of states.
    , tableDelta :: String
    -- ^ The next state of each transition.
    , tableLambda :: String
    -- ^ The output of each transition.
    }
    deriving (Show, Eq)

{- | Encode a 'ParsedMealy' as a 'MealyTable'.

  Returns @'Left' err@ if some state has no transition, or more than one
  distinct transition, for some input, or if the automaton is too large for
  the encoding (every number must be a code point below @0xD800@).
-}
mealyTable :: ParsedMealy -> Either String MealyTable
mealyTable pm
    | length stateNames > maxCode || length (parsedOutputs pm) > maxCode =
        Left ("Automaton is too large for the table encoding (at most " ++ show maxCode ++ " states and outputs)")
    | not (null conflicts) =
        Left ("Nondeterministic automaton:\n" ++ unlines (map describeConflict conflicts))
    | not (null missing) =
        Left $
            "Incomplete automaton: "
                ++ show (length missing)
                ++ " missing transition(s), e.g.\n"
                ++ unlines (map describeMissing (take 10 missing))
    | otherwise =
        Right
            MealyTable
                { tableStates = length stateNames
                , tableDelta = map (chr . fst) entries
                , tableLambda = map (chr . snd) entries
                }
  where
    maxCode = 0xD800
    stateNames = parsedStates pm
    inputNames = parsedInputs pm
    stateIdx = Map.fromList (zip stateNames [0 :: Int ..])
    inputIdx = Map.fromList (zip inputNames [0 :: Int ..])
    outputIdx = Map.fromList (zip (parsedOutputs pm) [0 :: Int ..])

    -- Every name is in its index map, since all three lists are built from
    -- 'parsedTrans' by 'parseDot'.
    byKey =
        Map.fromListWith
            Set.union
            [ ((stateIdx Map.! src, inputIdx Map.! inp), Set.singleton (stateIdx Map.! dst, outputIdx Map.! out))
            | (src, inp, dst, out) <- parsedTrans pm
            ]
    conflicts = [(k, Set.toList ts) | (k, ts) <- Map.toList byKey, Set.size ts > 1]
    keys = [(s, i) | s <- [0 .. length stateNames - 1], i <- [0 .. length inputNames - 1]]
    missing = filter (`Map.notMember` byKey) keys
    entries = [t | k <- keys, t <- take 1 (foldMap Set.toList (Map.lookup k byKey))]

    nameOf names = \x -> Map.findWithDefault "?" x (Map.fromList (zip [0 :: Int ..] names))
    stateName = nameOf stateNames
    inputName = nameOf inputNames
    outputName = nameOf (parsedOutputs pm)
    describeMissing (s, i) = "  state " ++ show (stateName s) ++ ", input " ++ show (inputName i)
    describeConflict ((s, i), ts) =
        describeMissing (s, i)
            ++ " → "
            ++ intercalate ", " [show (stateName d) ++ " / " ++ show (outputName o) | (d, o) <- ts]

-- ---------------------------------------------------------------------------
-- Code generator
-- ---------------------------------------------------------------------------

{- | Generate a Haskell module from a 'ParsedMealy'.

  @generateModule modName valName pm@ produces source for a module named
  @modName@ containing:

  * A @data \<modName\>Input@ type whose constructors are the sanitized input
    symbols, deriving @Show, Eq, Ord, Enum, Bounded@.
  * A @data \<modName\>Output@ type, similarly for output symbols.
  * A value @valName :: MealyAutomaton Int \<modName\>Input \<modName\>Output@,
    built with 'Haal.Automaton.MealyAutomaton.mkMealyAutomatonTable' from the
    'MealyTable' of @pm@, preceded by a comment listing every transition.

  Returns @'Left' err@ if two distinct symbols sanitize to the same
  constructor name, or if 'mealyTable' rejects the automaton.
-}
generateModule :: String -> String -> ParsedMealy -> Either String String
generateModule modName valName pm = do
    inputCons <- sanitizeAll "In_" "input" (parsedInputs pm)
    outputCons <- sanitizeAll "Out_" "output" (parsedOutputs pm)
    table <- mealyTable pm
    let n = tableStates table
        k = length inputCons
        modSuffix = reverse . takeWhile (/= '.') . reverse $ modName
        inputType = modSuffix ++ "Input"
        outputType = modSuffix ++ "Output"
    return $
        unlines $
            [ "-- Generated by haal-gen. Do not edit manually."
            , "module " ++ modName
            , "    ( " ++ inputType ++ " (..)"
            , "    , " ++ outputType ++ " (..)"
            , "    , " ++ valName
            , "    ) where"
            , ""
            , "import Haal.Automaton.MealyAutomaton (MealyAutomaton, mkMealyAutomatonTable)"
            , ""
            , "data " ++ inputType
            ]
                ++ enumDecl inputCons
                ++ [ ""
                   , "data " ++ outputType
                   ]
                ++ enumDecl outputCons
                ++ [""]
                ++ transitionComment inputCons outputCons table
                ++ [ valName ++ " :: MealyAutomaton Int " ++ inputType ++ " " ++ outputType
                   , valName ++ " ="
                   , "    case mkMealyAutomatonTable " ++ show n ++ " 0 deltaTable lambdaTable of"
                   , "        Right m -> m"
                   , "        Left err -> error (\"haal-gen: invalid transition table: \" ++ err)"
                   , "  where"
                   , "    deltaTable ="
                   ]
                ++ stringRows k (tableDelta table)
                ++ ["    lambdaTable ="]
                ++ stringRows k (tableLambda table)

-- ---------------------------------------------------------------------------
-- Code generation helpers
-- ---------------------------------------------------------------------------

enumDecl :: [String] -> [String]
enumDecl [] = ["    deriving (Show, Eq, Ord, Enum, Bounded)"]
enumDecl (c : cs) =
    ["    = " ++ c]
        ++ map ("    | " ++) cs
        ++ ["    deriving (Show, Eq, Ord, Enum, Bounded)"]

{- | A block comment listing every transition of the table, one state at a
  time, so that the generated module stays readable.
-}
transitionComment :: [String] -> [String] -> MealyTable -> [String]
transitionComment inputCons outputCons table =
    ["{- Transitions (state  input -> next state / output):"]
        ++ concat (zipWith stateLines [0 :: Int ..] (chunksOf k entries))
        ++ ["-}"]
  where
    k = length inputCons
    entries = zip (tableDelta table) (tableLambda table)
    width = maximum (0 : map length inputCons)
    stateLines s row =
        [ "    " ++ pad 5 (if j == 0 then show s else "") ++ pad width inp ++ " -> " ++ show (ord d) ++ " / " ++ out
        | (j, inp, (d, o)) <- zip3 [0 :: Int ..] inputCons row
        , out <- take 1 (drop (ord o) outputCons)
        ]
    pad w str = str ++ replicate (w - length str + 1) ' '

{- | Render a table as an indented string literal with one row of @k@
  entries per line, joined by string gaps. Every entry is written as a
  numeric escape, so that each row reads as a list of numbers.
-}
stringRows :: Int -> String -> [String]
stringRows k str = case chunksOf k str of
    [] -> ["        \"\""]
    rows ->
        [ "        " ++ open ++ concatMap escape row ++ close
        | (j, row) <- zip [0 :: Int ..] rows
        , let open = if j == 0 then "\"" else "\\"
              close = if j == length rows - 1 then "\"" else "\\"
        ]
  where
    escape c = '\\' : show (ord c)

chunksOf :: Int -> [a] -> [[a]]
chunksOf k xs
    | k <= 0 = []
    | otherwise = case splitAt k xs of
        ([], _) -> []
        (chunk, rest) -> chunk : chunksOf k rest

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

{- | Remove duplicates, keeping the first occurrence of each element, in
  @O(n log n)@. Symbols and states keep the order in which they first appear
  in the DOT file, so that regenerating a model keeps its constructor order
  and state numbering.
-}
ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
        | x `Set.member` seen = go seen xs
        | otherwise = x : go (Set.insert x seen) xs

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
