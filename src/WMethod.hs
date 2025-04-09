module WMethod (
    WMethod (..),
    wmethod,
) where

import BlackBox (Automaton, SUL, accessSequences, globalCharacterizingSet, inputs, reset, states, walk)
import Control.Monad (replicateM)
import Data.Data (Data)
import qualified Data.Map as Map
import qualified Data.Set as Set
import EquivalenceOracle (EquivalenceOracle, findCex)

newtype WMethod = WMethod {depth :: Int}

wmethod :: (Automaton aut, Ord i, Data i, Ord s, Data s, Eq o, Bounded s, SUL sul) => WMethod -> aut i o s -> sul i o s -> Maybe ([i], [o])
wmethod (WMethod{depth = d}) aut sul = execute suite
  where
    alphabet = Set.toList $ inputs aut
    numStates = length $ Set.toList $ states aut
    accessSeqs = accessSequences aut
    characterizingSet = Set.toList $ globalCharacterizingSet aut
    transitionCover = [a ++ [inp] | a <- Map.elems accessSeqs, inp <- alphabet]

    exploration = concatMap (`replicateM` alphabet) [1 .. d + numStates]
    suite = [acc ++ inter ++ char | inter <- exploration, acc <- transitionCover, char <- characterizingSet]

    execute [] = Nothing
    execute (s : ss) = if systemOut /= hypothOut then execute ss else Just (s, systemOut)
      where
        (_, systemOut) = walk (reset sul) s
        (_, hypothOut) = walk (reset aut) s

instance EquivalenceOracle WMethod where
    findCex = wmethod
