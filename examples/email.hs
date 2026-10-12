{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- \| Learn Mealy models of email address validators and look for addresses on
-- which they disagree.
--
-- \* @email-validate@: Haskell's attoparsec parser aiming at RFC 5322.
-- \* The WHATWG rule behind @\<input type="email"\>@, written out by hand below.
-- \* Chromium's @\<input type="email"\>@ itself, queried through a Node driver
--   (@examples/email-driver@; run @npm install@ there first).
--
-- Each validator becomes a SUL over an abstract alphabet: every input symbol is
-- one character class, concretised to one representative character. The output
-- at each step is the validator's verdict on the prefix read so far.

import Control.Monad (when)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import Haal.Automaton.MealyAutomaton (MealyAutomaton)
import Haal.BlackBox (Automaton (..), SUL (..), difference)
import Haal.EquivalenceOracle.WpMethod
import Haal.Experiment
import Haal.Learning.LMstar (LMstarConfig (Star), mkLMstar)
import Haal.Statistics (Event (..))
import Numeric (showHex)
import System.IO (BufferMode (LineBuffering), Handle, hClose, hGetLine, hPutStrLn, hSetBuffering, stdout)
import System.Process (CreateProcess (..), StdStream (..), proc, waitForProcess, withCreateProcess)
import System.Random (mkStdGen)
import qualified Text.Email.Validate as EV

--------------------------------------------------------------------------------
-- Abstract alphabet
--------------------------------------------------------------------------------

{- | Character classes. They must be a common refinement for every validator
compared, or a class can hide a difference. Non-ASCII input is left out for now,
and so are parentheses: @email-validate@ accepts RFC 5322 comments, which nest,
so its language is not regular and learning never converges (every round finds
one more level of nesting, e.g. @a\@a(())@).
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

--------------------------------------------------------------------------------
-- Validators
--------------------------------------------------------------------------------

emailValidate :: String -> Bool
emailValidate = EV.isValid . BS.pack

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

{- | Chromium, through the driver process. A query is one line each way: the
concrete word as a JSON string, and back one character per nonempty prefix
(@1@ valid, @0@ invalid). 'chBuffer' holds the input read so far, reversed,
for the rare plain 'step'.
-}
data Chromium i o = Chromium
    { chIn :: Handle
    , chOut :: Handle
    , chEncode :: i -> Char
    , chDecode :: Char -> Maybe o
    , chBuffer :: [i]
    }

instance SUL Chromium IO where
    step c i = do
        let buf = i : chBuffer c
        os <- ask c (reverse buf)
        case reverse os of
            o : _ -> pure (c{chBuffer = buf}, o)
            [] -> fail "driver returned no verdicts"
    reset c = pure c{chBuffer = []}

    query c xs = ask c xs

-- | Send a word to the driver and read its verdicts on all nonempty prefixes.
ask :: Chromium i o -> [i] -> IO [o]
ask _ [] = pure []
ask c xs = do
    hPutStrLn (chIn c) (jsonString (map (chEncode c) xs))
    answer <- hGetLine (chOut c)
    traverse decode answer
  where
    decode x = maybe (fail ("unexpected driver output: " ++ show x)) pure (chDecode c x)

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

-- | Start the driver, run an action with it, and shut it down (closing its stdin closes the browser).
withChromium :: (Chromium Symbol Bool -> IO a) -> IO a
withChromium act =
    withCreateProcess (proc "node" ["examples/email-driver/driver.mjs"]){std_in = CreatePipe, std_out = CreatePipe} $
        \mIn mOut _ ph -> case (mIn, mOut) of
            (Just hin, Just hout) -> do
                hSetBuffering hin LineBuffering
                result <- act (Chromium hin hout concretise decodeVerdict [])
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
    config = RandomWpMethodConfig{rwpGen = mkStdGen 42, rwpExpected = 10, rwpMin = 1, rwpLimit = 20000}

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

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    let ev = mkValidator emailValidate
        wg = mkValidator whatwg
    evModel <- learnSUL "email-validate" ev
    wgModel <- learnSUL "WHATWG (hand-written)" wg
    withChromium $ \ch -> do
        chModel <- learnSUL "Chromium" ch
        let models =
                [ Learned "email-validate" evModel (query ev)
                , Learned "WHATWG" wgModel (query wg)
                , Learned "Chromium" chModel (query ch)
                ]
        sequence_ [compareModels a b | (k, a) <- zip [0 :: Int ..] models, b <- drop (k + 1) models]
