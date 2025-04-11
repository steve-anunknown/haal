module WMethod (
    WMethod (..),
    wmethod,
    wmethodSuite,
) where

import BlackBox (Automaton, SUL, accessSequences, globalCharacterizingSet, inputs, reset, step, walk)
import Control.Monad (replicateM)
import Data.Data (Data)
import qualified Data.Map as Map
import qualified Data.Set as Set
import EquivalenceOracle (EquivalenceOracle, findCex, testSuite)

newtype WMethod = WMethod {depth :: Int} deriving(Show, Eq)

wmethodSuite ::
    ( Automaton aut
    , Ord i
    , Data i
    , Ord s
    , Data s
    , Eq o
    ) =>
    WMethod ->
    aut i o s ->
    [[i]]
wmethodSuite (WMethod{depth = d}) aut = suite
  where
    alphabet = Set.toList $ inputs aut
    accessSeqs = accessSequences aut
    characterizingSet = Set.toList $ globalCharacterizingSet aut
    transitionCover = [a ++ [inp] | a <- Map.elems accessSeqs, inp <- alphabet]
    suite =
        concat
            [ [acc ++ middle ++ char | acc <- transitionCover, char <- characterizingSet]
            | fixed <- [0 .. d]
            , middle <- replicateM fixed alphabet
            ]

wmethod ::
    ( Automaton aut
    , Ord i
    , Data i
    , Ord s
    , Data s
    , Eq o
    , Bounded s
    , SUL sul
    ) =>
    WMethod ->
    aut i o s ->
    sul i o s ->
    Maybe ([i], [o])
wmethod (WMethod{depth = d}) aut sul = execute suite
  where
    suite = wmethodSuite (WMethod d) aut

    pairwiseWalk _ _ [] = True
    pairwiseWalk theSul theAut (s : ss) = (out1 == out2) && pairwiseWalk sul' aut' ss
      where
        (sul', out1) = step theSul s
        (aut', out2) = step theAut s

    execute [] = Nothing
    execute (s : ss) = if continue then execute ss else Just (s, snd $ walk (reset sul) s)
      where
        continue = pairwiseWalk (reset sul) (reset aut) s

instance EquivalenceOracle WMethod where
    testSuite = wmethodSuite
    findCex = wmethod
