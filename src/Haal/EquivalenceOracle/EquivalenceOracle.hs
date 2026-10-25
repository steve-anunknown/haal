module Haal.EquivalenceOracle.EquivalenceOracle (
    EquivalenceOracle (..),
    execute,
    findCex,
) where

import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (ReaderT, ask, lift)
import Haal.BlackBox

{- | The 'EquivalenceOracle' type class defines the interface for equivalence oracles.
Instances of this class should provide methods to generate a test suite
-}
class EquivalenceOracle or where
    testSuite ::
        ( Automaton aut s
        , FiniteOrd i
        , FiniteOrd s
        , Eq o
        ) =>
        or ->
        aut s i o ->
        (or, [[i]])

{- | The 'execute' function executes the test suite of an oracle, given a SUL and an automaton.
Every test case is run from the initial state of both the SUL and the automaton. It returns
the first test case on which they disagree, together with the outputs of the SUL, or a pair
of empty lists if they agree on every test case.
-}
execute ::
    ( SUL sul m
    , Automaton aut s
    , Ord i
    , Eq o
    ) =>
    sul i o ->
    aut s i o ->
    [[i]] ->
    m ([i], [o])
execute _ _ [] = return ([], [])
execute theSul theAut (s : ss) = do
    out <- queryChecked theSul s
    if out == runIdentity (query theAut s)
        then execute theSul theAut ss
        else return (s, out)

{- | The 'findCex' function executes the test suite of each oracle to the automaton
and SUL.
-}
findCex ::
    ( SUL sul m
    , Automaton aut s
    , EquivalenceOracle or
    , FiniteOrd i
    , FiniteOrd s
    , Eq o
    ) =>
    or ->
    aut s i o ->
    ReaderT (sul i o) m (or, ([i], [o]))
findCex oracle aut = do
    sul <- ask
    let (oracle', theSuite) = testSuite oracle aut
    (cin, cout) <- lift $ execute sul aut theSuite
    return (oracle', (cin, cout))
