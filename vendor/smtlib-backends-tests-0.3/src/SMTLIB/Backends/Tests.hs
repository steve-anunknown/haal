{-# LANGUAGE OverloadedStrings #-}

-- | A module providing functions useful for testing a backend for SimpleSMT.
module SMTLIB.Backends.Tests
  ( testBackend,
    Src.Source (..),
    Src.sources,
  )
where

import SMTLIB.Backends (Backend, QueuingFlag (..), command, initSolver)
import qualified SMTLIB.Backends.Tests.Sources as Src
import Test.Tasty
import Test.Tasty.HUnit

-- | Test a backend by using it to run a list of examples.
testBackend ::
  -- | The name of the test group.
  String ->
  -- | A list of examples on which to run the backend.
  [Src.Source] ->
  -- | A function that should create a backend, run a given
  -- computation and release the backend's resources.
  ((Backend -> Assertion) -> Assertion) ->
  TestTree
testBackend name sources with =
  testGroup name $ do
    queuing <- [NoQueuing, Queuing]
    return
      $ testGroup
        ( case queuing of
            Queuing -> "queuing"
            NoQueuing -> "no queuing"
        )
      $ do
        source <- sources
        return $
          testCase (Src.name source) $
            with $ \backend -> do
              solver <- initSolver queuing backend
              Src.run source solver
              -- ensure the sources consisting only of queued commands also run
              _ <- command solver "(get-info :name)"
              return ()
