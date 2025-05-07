{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Examples (examples) where

import Control.Concurrent.Async (async)
import Control.Exception (SomeException (SomeException), catch)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy.Char8 as LBS
import SMTLIB.Backends (QueuingFlag (..), command, command_, flushQueue, initSolver)
import qualified SMTLIB.Backends.Process as Process
import System.IO (BufferMode (LineBuffering), hClose, hGetLine, hSetBuffering)
import System.Process (waitForProcess)
import Test.Tasty
import Test.Tasty.HUnit

-- | The examples for the 'Process' backend (running solvers as external
-- processes).
examples :: [TestTree]
examples =
  [ testCase "basic use" basicUse,
    testCase "setting options" setOptions,
    testCase "managing the underlying process" underlyingProcess,
    testCase "flushing the queue" flushing
  ]

-- | Basic use of the 'Process' backend.
basicUse :: IO ()
basicUse =
  -- 'Process.with' runs a computation using the 'Process' backend
  Process.with
    -- the default configuration uses Z3 as an external process and disables logging
    Process.defaultConfig
    $ \handle -> do
      -- first, we make the process handle into an actual backend
      let backend = Process.toBackend handle
      -- then, we create a solver out of the backend
      -- we enable queuing (it's faster !)
      solver <- initSolver Queuing backend
      -- we send a basic command to the solver and ignore the response
      -- we can write the command as a simple string because we have enabled the
      -- OverloadedStrings pragma
      _ <- command solver "(get-info :name)"
      -- note how there is no need to send an @(exit)@ command, this is already
      -- handled by the 'Process.with' function
      return ()

-- | An example of how to change the default settings of the 'Process' backend.
setOptions :: IO ()
setOptions =
  -- here we use a custom-made configuration
  let myConfig =
        Process.defaultConfig
          { Process.exe = "z3",
            Process.args = ["-in", "solver.timeout=10000"]
          }
   in Process.with myConfig $ \handle -> do
        solver <- initSolver Queuing $ Process.toBackend handle
        _ <- command solver "(get-info :name)"
        return ()

-- | An example of how to get the backend's underlying process and manage it
-- manually, for instance when you want to monitor the process' error channel.
underlyingProcess :: IO ()
underlyingProcess = do
  -- since the 'Process' module exposes its 'Handle' datatype entirely, we have
  -- direct access to the process and its I/O channels
  -- this can in particular be useful if you want to monitor the process' errors
  -- that it prints on its error channel
  -- otherwise, it's unlikely you'll need this as the library already provides
  -- enough bindings for common needs and chooses the process' settings to
  -- ensure the communication with solvers is as fast as possible
  --
  -- we'll close the process manually so we just launch it with 'Process.new'
  -- instead of using `Process.with`
  Process.Handle {hMaybeErr = Just hErr, ..} <- Process.new Process.defaultConfig
  -- the main use of accessing the internals of the 'Process.Handle' is to monitor
  -- the process' error channel
  _ <- async $ forever (hGetLine hErr >>= putStrLn) `catch` \SomeException {} -> return ()
  -- we can also change the settings of the underlying process
  -- for instance here we change the buffering mode of the process' input channel
  hSetBuffering hIn LineBuffering
  -- the default way to close the process is to use 'Process.close', which sends
  -- it a SIGTERM
  -- if you don't like this, you can always send it an @(exit)@ command and wait
  -- for it to end, but make sure you also release its other resources
  LBS.hPutStrLn hIn "(exit)"
  mapM_ hClose [hIn, hOut, hErr]
  _ <- waitForProcess process
  return ()

-- | An example on how to force the content of the queue to be evaluated.
flushing :: IO ()
flushing = do
  -- sometimes you want to use 'Queuing' mode but still force some commands not
  -- producing any output to be evaluated
  -- in that case, using 'command' would lead to your program hanging as it waits
  -- for a response from the solver that never comes
  -- the solution is to use the 'command_' function and then to flush the queue
  Process.with Process.defaultConfig $ \handle -> do
    -- this example only makes sense in queuing mode
    solver <- initSolver Queuing $ Process.toBackend handle
    -- add a command to the queue
    command_ solver "(assert true)"
    -- force the queue to be evaluated
    flushQueue solver
