module CombinedOracle (
    CombinedOracle (..),
) where

import Control.Applicative ((<|>))
import Experiment (EquivalenceOracle (..))

data CombinedOracle a b = CombinedOracle a b deriving (Show, Eq)

instance
    ( EquivalenceOracle a
    , EquivalenceOracle b
    ) =>
    EquivalenceOracle (CombinedOracle a b)
    where
    testSuiteSize (CombinedOracle or1 or2) aut = testSuiteSize or1 aut + testSuiteSize or2 aut
    testSuite (CombinedOracle or1 or2) aut = testSuite or1 aut ++ testSuite or2 aut
    findCex (CombinedOracle or1 or2) aut = do
        cex1 <- findCex or1 aut
        cex2 <- findCex or2 aut
        return (cex1 <|> cex2)
