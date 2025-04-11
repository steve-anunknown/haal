module CombinedOracle (
    CombinedOracle (..),
) where

import Control.Applicative ((<|>))
import EquivalenceOracle (EquivalenceOracle (..))

data CombinedOracle a b = CombinedOracle a b deriving (Show, Eq)

instance
    ( EquivalenceOracle a
    , EquivalenceOracle b
    ) =>
    EquivalenceOracle (CombinedOracle a b)
    where
    testSuite (CombinedOracle or1 or2) aut = testSuite or1 aut ++ testSuite or2 aut
    findCex (CombinedOracle or1 or2) aut sul = findCex or1 aut sul <|> findCex or2 aut sul
