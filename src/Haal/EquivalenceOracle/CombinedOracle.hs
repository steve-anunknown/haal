-- | This module implements a combined equivalence oracle for two oracles.
module Haal.EquivalenceOracle.CombinedOracle (
    CombinedOracle (..),
) where

import Haal.Experiment

{- | The 'CombinedOracle' type is a data type for combining two equivalence oracles.
It is used to chain multiple oracles together by first exhausting the test suite
of the first oracle and then using the second oracle.
-}
data CombinedOracle a b = CombinedOracle a b deriving (Show, Eq)

instance
    ( EquivalenceOracle a
    , EquivalenceOracle b
    ) =>
    EquivalenceOracle (CombinedOracle a b)
    where
    testSuite (CombinedOracle or1 or2) aut =
        let (or1', testSuite1) = testSuite or1 aut
            (or2', testSuite2) = testSuite or2 aut
         in (CombinedOracle or1' or2', testSuite1 ++ testSuite2)
