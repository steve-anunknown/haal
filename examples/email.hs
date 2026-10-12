{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- \| Learn Mealy models of email address validators and look for addresses on
-- which they disagree.
--
-- \* @email-validate@: Haskell's attoparsec parser aiming at RFC 5322.
-- \* The WHATWG rule behind @\<input type="email"\>@, written out by hand below.
-- \* Chromium's and Firefox's @\<input type="email"\>@ itself, queried through a
--   Node driver (@examples/email-driver@; run @npm install@ there first).
--
-- Each validator becomes a SUL over an abstract alphabet: every input symbol is
-- one character class, concretised to one representative character. The output
-- at each step is the validator's verdict on the prefix read so far.

import Control.Monad (when)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Haal.Automaton.MealyAutomaton (MealyAutomaton, mkMealyAutomaton2)
import Haal.BlackBox (Automaton (..), SUL (..), difference)
import Haal.Dot (ParsedMealy (..), mealyToDot, parseDot)
import Haal.EquivalenceOracle.WpMethod
import Haal.Experiment
import Haal.Learning.LMstar (LMstarConfig (Star), mkLMstar)
import Haal.Statistics (Event (..))
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((<.>), (</>))
import System.IO (BufferMode (LineBuffering), Handle, hClose, hGetLine, hPutStrLn, hSetBuffering, hSetEncoding, stdout, utf8)
import System.Process (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)
import System.Random (mkStdGen)
import qualified Text.Email.Validate as EV

--------------------------------------------------------------------------------
-- Abstract alphabet
--------------------------------------------------------------------------------

{- | Character classes. They must be a common refinement for every validator
compared, or a class can hide a difference. Parentheses are left out:
@email-validate@ accepts RFC 5322 comments, which nest, so its language is not
regular and learning never converges (every round finds one more level of
nesting, e.g. @a\@a(())@).

'NonAscii' is aimed at internationalised domains, which browsers convert to
punycode before validating; it can appear anywhere in the address, though.
-}
data Symbol
    = Letter
    | Digit
    | Dot
    | At
    | Hyphen
    | Plus -- stands for the other atext characters
    | Quote
    | Backslash
    | Space
    | LBracket
    | RBracket
    | NonAscii -- a letter outside ASCII
    deriving (Show, Eq, Ord, Enum, Bounded)

concretise :: Symbol -> Char
concretise Letter = 'a'
concretise Digit = '1'
concretise Dot = '.'
concretise At = '@'
concretise Hyphen = '-'
concretise Plus = '+'
concretise Quote = '"'
concretise Backslash = '\\'
concretise Space = ' '
concretise LBracket = '['
concretise RBracket = ']'
concretise NonAscii = '\252' -- ü

--------------------------------------------------------------------------------
-- Validators
--------------------------------------------------------------------------------

emailValidate :: String -> Bool
emailValidate = EV.isValid . BL.toStrict . BB.toLazyByteString . BB.stringUtf8

{- | The WHATWG "valid email address" rule:

> ^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$

The 63-character label limit is left out: the oracle never reaches such
lengths, and it would only add a counter chain to the model.
-}
whatwg :: String -> Bool
whatwg s = case break (== '@') s of
    (local@(_ : _), '@' : domain) -> all localChar local && all label (splitDots domain)
    _ -> False
  where
    alnum c = isAsciiLower c || isAsciiUpper c || isDigit c
    localChar c = alnum c || c `elem` ".!#$%&'*+/=?^_`{|}~-"
    label l = case (l, reverse l) of
        (c : _, d : _) -> alnum c && alnum d && all (\x -> alnum x || x == '-') l
        _ -> False
    splitDots str = case break (== '.') str of
        (l, '.' : rest) -> l : splitDots rest
        (l, _) -> [l]

--------------------------------------------------------------------------------
-- SUL
--------------------------------------------------------------------------------

-- | A pure validator over abstract inputs; 'buffer' holds the input read so far, reversed.
data Validator i o = Validator
    { verdict :: [i] -> o
    , buffer :: [i]
    }

instance (Monad m) => SUL Validator m where
    step v i = let buf = i : buffer v in pure (v{buffer = buf}, verdict v (reverse buf))
    reset v = pure v{buffer = []}

    -- one verdict per nonempty prefix, without replaying through 'step'
    query v xs = pure [verdict v (take n xs) | n <- [1 .. length xs]]

mkValidator :: (String -> Bool) -> Validator Symbol Bool
mkValidator f = Validator (f . map concretise) []

{- | A browser, through the driver process. A query is one line each way: the
concrete word as a JSON string, and back one character per nonempty prefix
(@1@ valid, @0@ invalid). 'brBuffer' holds the input read so far, reversed,
for the rare plain 'step'.
-}
data Browser i o = Browser
    { brIn :: Handle
    , brOut :: Handle
    , brEncode :: i -> Char
    , brDecode :: Char -> Maybe o
    , brBuffer :: [i]
    }

instance SUL Browser IO where
    step c i = do
        let buf = i : brBuffer c
        os <- ask c (reverse buf)
        case reverse os of
            o : _ -> pure (c{brBuffer = buf}, o)
            [] -> fail "driver returned no verdicts"
    reset c = pure c{brBuffer = []}

    query = ask

-- | Send a word to the driver and read its verdicts on all nonempty prefixes.
ask :: Browser i o -> [i] -> IO [o]
ask _ [] = pure []
ask c xs = do
    hPutStrLn (brIn c) (jsonString (map (brEncode c) xs))
    answer <- hGetLine (brOut c)
    traverse decode answer
  where
    decode x = maybe (fail ("unexpected driver output: " ++ show x)) pure (brDecode c x)

-- | A JSON string literal (the driver parses each line with @JSON.parse@).
jsonString :: String -> String
jsonString s = "\"" ++ concatMap escape s ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c
        | c < ' ' = "\\u" ++ replicate (4 - length hex) '0' ++ hex
        | otherwise = [c]
      where
        hex = showHex (fromEnum c) ""

{- | Start the driver for a browser (@chromium@ or @firefox@), run an action
with it, and shut it down (closing its stdin closes the browser).
-}
withBrowser :: String -> (Browser Symbol Bool -> IO a) -> IO a
withBrowser browser act =
    withCreateProcess (proc "node" ["examples/email-driver/driver.mjs", browser]){std_in = CreatePipe, std_out = CreatePipe} $
        \mIn mOut _ ph -> case (mIn, mOut) of
            (Just hin, Just hout) -> do
                hSetBuffering hin LineBuffering
                mapM_ (`hSetEncoding` utf8) [hin, hout]
                result <- act (Browser hin hout concretise decodeVerdict [])
                hClose hin
                _ <- waitForProcess ph
                pure result
            _ -> fail "could not open pipes to the driver"
  where
    decodeVerdict '1' = Just True
    decodeVerdict '0' = Just False
    decodeVerdict _ = Nothing

--------------------------------------------------------------------------------
-- Experiment
--------------------------------------------------------------------------------

type Model = MealyAutomaton Int Symbol Bool

oracle :: RandomWpMethod
oracle = either error id (mkRandomWpMethod config)
  where
    config = RandomWpMethodConfig{rwpGen = mkStdGen 17, rwpExpected = 10, rwpMin = 1, rwpLimit = 20000}

-- | Learn a SUL, printing progress: query counts, hypotheses and counterexamples.
learnSUL :: (SUL sul IO) => String -> sul Symbol Bool -> IO Model
learnSUL name sul = do
    putStrLn name
    count <- newIORef (0 :: Int)
    let emit :: Event MealyAutomaton Symbol Bool -> IO ()
        emit (Queried _ _) = do
            modifyIORef' count (+ 1)
            n <- readIORef count
            when (n `mod` 100000 == 0) $ putStrLn ("  " ++ show n ++ " queries")
        emit (PhaseChanged p) = putStrLn ("  phase: " ++ show p)
        emit (Hypothesis h) = putStrLn ("  hypothesis with " ++ show (Set.size (states h)) ++ " states")
        emit (Counterexample w) = putStrLn ("  counterexample: " ++ show (map concretise w))
    model <- runExperimentT (experimentWith emit (mkLMstar Star) oracle) sul
    n <- readIORef count
    putStrLn ("  done after " ++ show n ++ " queries")
    pure model

-- | A learned model, with the system it was learned from, to check its verdicts.
data Learned = Learned
    { learnedName :: String
    , learnedFile :: FilePath
    , learnedModel :: Model
    , learnedSystem :: [Symbol] -> IO [Bool]
    }

{- | Print the shortest address on which two models differ, with the verdicts of
the real systems: if those agree, one of the models is wrong.
-}
compareModels :: Learned -> Learned -> IO ()
compareModels a b = do
    putStrLn (learnedName a ++ " vs " ++ learnedName b)
    case difference (learnedModel a) (learnedModel b) of
        Nothing -> putStrLn "  the learned models agree"
        Just w -> do
            putStrLn ("  shortest address on which they differ: " ++ show (map concretise w))
            mapM_ (report w) [a, b]
  where
    report w l = do
        os <- learnedSystem l w
        putStrLn ("    " ++ learnedName l ++ ": " ++ maybe "no verdict" show (lastMaybe os))
    lastMaybe = foldl (\_ x -> Just x) Nothing

--------------------------------------------------------------------------------
-- Saved models
--------------------------------------------------------------------------------

-- | Where the models are saved, relative to the working directory.
modelDir :: FilePath
modelDir = "email-models"

{- | Read a model saved by 'saveModel'. States are numbered in the order
'parseDot' lists them, so the initial state is @0@.
-}
loadModel :: FilePath -> IO (Either String Model)
loadModel path = do
    src <- readFile path
    pure (parseDot src >>= fromParsed)
  where
    fromParsed pm = do
        let stateIdx = Map.fromList (zip (parsedStates pm) [0 :: Int ..])
            symbols = Map.fromList [(show x, x) | x <- [minBound .. maxBound :: Symbol]]
            outputs = Map.fromList [("True", True), ("False", False)]
            look what m k = maybe (Left ("unknown " ++ what ++ " " ++ show k)) Right (Map.lookup k m)
        table <-
            Map.fromList
                <$> traverse
                    ( \(src, i, dst, o) -> do
                        k <- (,) <$> look "state" stateIdx src <*> look "input" symbols i
                        v <- (,) <$> look "state" stateIdx dst <*> look "output" outputs o
                        pure (k, v)
                    )
                    (parsedTrans pm)
        let sts = Map.elems stateIdx
            complete = and [Map.member (q, x) table | q <- sts, x <- [minBound .. maxBound]]
        if complete
            then -- the default is never used: every transition was checked above
                Right (mkMealyAutomaton2 (\q x -> Map.findWithDefault (q, False) (q, x) table) (Set.fromList sts) 0)
            else Left "incomplete model"

saveModel :: FilePath -> Model -> IO ()
saveModel path m = either (fail . ("cannot save model: " ++)) (writeFile path) (mealyToDot m)

{- | Compare a model with the one saved by the previous run, if any, then save it
in its place.
-}
checkAndSave :: Learned -> IO ()
checkAndSave l = do
    let path = modelDir </> learnedFile l <.> "dot"
    exists <- doesFileExist path
    if not exists
        then putStrLn ("  " ++ learnedName l ++ ": no saved model")
        else do
            saved <- loadModel path
            putStrLn $
                "  " ++ learnedName l ++ ": " ++ case saved of
                    Left err -> "cannot read saved model (" ++ err ++ ")"
                    Right old -> case difference old (learnedModel l) of
                        Nothing -> "same as the saved model"
                        Just w -> "differs from the saved model on " ++ show (map concretise w)
    saveModel path (learnedModel l)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    let ev = mkValidator emailValidate
        wg = mkValidator whatwg
    evModel <- learnSUL "email-validate" ev
    wgModel <- learnSUL "WHATWG (hand-written)" wg
    withBrowser "chromium" $ \ch -> withBrowser "firefox" $ \ff -> do
        chModel <- learnSUL "Chromium" ch
        ffModel <- learnSUL "Firefox" ff
        let models =
                [ Learned "email-validate" "email-validate" evModel (query ev)
                , Learned "WHATWG" "whatwg" wgModel (query wg)
                , Learned "Chromium" "chromium" chModel (query ch)
                , Learned "Firefox" "firefox" ffModel (query ff)
                ]
        sequence_ [compareModels a b | (k, a) <- zip [0 :: Int ..] models, b <- drop (k + 1) models]
        createDirectoryIfMissing True modelDir
        putStrLn ("Saved models (" ++ modelDir ++ ")")
        mapM_ checkAndSave models
