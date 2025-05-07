{-# LANGUAGE OverloadedStrings #-}

module EdgeCases (edgeCases) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy.Char8 as LBS
import SMTLIB.Backends as SMT
import qualified SMTLIB.Backends.Process as Process
import System.Process (terminateProcess, waitForProcess)
import Test.Tasty
import Test.Tasty.HUnit

edgeCases :: [TestTree]
edgeCases =
  [ testCase "Piling up stopping procedures" pileUpStops,
    testCase "Sending an empty command" emptyCommand,
    testCase "Sending a command expecting no response" commandNoResponse
  ]

-- | It's possible to accumulate procedures that stop the backend without
-- hanging or crashing the program.
pileUpStops :: IO ()
pileUpStops = Process.with Process.defaultConfig $ \handle -> do
  let backend = Process.toBackend handle
      process = Process.process handle
  SMT.send_ backend "(exit)"
  _ <- waitForProcess process
  terminateProcess process

-- | Upon processing an empty command, the backend will not respond.
emptyCommand :: IO ()
emptyCommand = checkNoResponse ""

-- | Upon processing a command producing no output, the backend will not
-- respond, not even with an empty line.
commandNoResponse :: IO ()
commandNoResponse = checkNoResponse "(set-option :print-success false)"

checkNoResponse :: Builder -> IO ()
checkNoResponse cmd = do
  Just response <- Process.with Process.defaultConfig $ \handle -> do
    let backend = Process.toBackend handle
    -- using 'SMT.send' instead would hang the program
    SMT.send_ backend cmd
    -- (check-sat) will produce "sat"
    LBS.stripSuffix "sat" <$> SMT.send backend "(check-sat)"
  assertEqual "expected no response" "" response
