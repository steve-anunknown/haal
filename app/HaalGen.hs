-- | haal-gen: generate a Haskell module from a Mealy automaton in DOT format.
--
-- Usage:
--   haal-gen <dotfile> <ModuleName> <valueName>
--
-- The generated module is written to stdout.
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import Haal.Dot (parseDot, generateModule, parsedWarnings)

helpText :: String
helpText = unlines
    [ "haal-gen - generate a Haskell module from a Mealy automaton in DOT format"
    , ""
    , "Usage:"
    , "  haal-gen <dotfile> <ModuleName> <valueName>"
    , ""
    , "Arguments:"
    , "  <dotfile>     Path to a DOT file (AALpy or LearnLib format)"
    , "  <ModuleName>  Fully qualified Haskell module name (e.g. My.Module.Name)"
    , "  <valueName>   Name for the generated automaton value (e.g. myAutomaton)"
    , ""
    , "Output:"
    , "  Haskell source is written to stdout; warnings and errors to stderr."
    , "  The generated module contains typed input/output ADTs and a"
    , "  MealyAutomaton value, ready to import and use with haal."
    , ""
    , "Example:"
    , "  haal-gen model.dot Haal.Models.Foo fooModel > src/Haal/Models/Foo.hs"
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"] -> putStr helpText >> exitSuccess
        [dotFile, modName, valName] -> run dotFile modName valName
        _ -> do
            hPutStrLn stderr "Usage: haal-gen <dotfile> <ModuleName> <valueName>"
            hPutStrLn stderr "Run 'haal-gen --help' for more information."
            exitFailure

run :: FilePath -> String -> String -> IO ()
run dotFile modName valName = do
    src <- readFile dotFile
    case parseDot src of
        Left err -> do
            hPutStrLn stderr ("haal-gen: parse error: " ++ err)
            exitFailure
        Right pm -> do
            mapM_ (hPutStrLn stderr . ("haal-gen: warning: " ++)) (parsedWarnings pm)
            case generateModule modName valName pm of
                Left err  -> do
                    hPutStrLn stderr ("haal-gen: " ++ err)
                    exitFailure
                Right hsrc -> putStr hsrc
