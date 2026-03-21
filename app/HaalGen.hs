-- | haal-gen: generate a Haskell module from a Mealy automaton in DOT format.
--
-- Usage:
--   haal-gen <dotfile> <ModuleName> <valueName>
--
-- The generated module is written to stdout.
module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Haal.Dot (parseDot, generateModule, parsedWarnings)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dotFile, modName, valName] -> run dotFile modName valName
        _ -> do
            hPutStrLn stderr "Usage: haal-gen <dotfile> <ModuleName> <valueName>"
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
