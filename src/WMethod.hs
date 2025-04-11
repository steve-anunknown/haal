module WMethod (
    WMethod (..),
    wmethod,
) where

import BlackBox (Automaton, SUL, accessSequences, globalCharacterizingSet, inputs, reset, states, walk, step)
import Control.Monad (replicateM)
import Data.Data (Data)
import qualified Data.Map as Map
import qualified Data.Set as Set
import EquivalenceOracle (EquivalenceOracle, findCex)

newtype WMethod = WMethod {depth :: Int}

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
    alphabet = Set.toList $ inputs aut
    numStates = length $ Set.toList $ states aut
    accessSeqs = accessSequences aut
    characterizingSet = Set.toList $ globalCharacterizingSet aut
    transitionCover = [a ++ [inp] | a <- Map.elems accessSeqs, inp <- alphabet]

    suite = concat [ [acc ++ middle ++ char | acc <- transitionCover, char <- characterizingSet]
                | fixed <- [0..d + numStates -1]
                , middle <- replicateM fixed alphabet ]

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
    findCex = wmethod
