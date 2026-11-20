{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Copyright: (c) 2025-2026 Joshua Moerman, Open Universiteit
-- SPDX-License-Identifier: BSD-3-Clause
--
-- This file is ported from https://git.cs.ou.nl/joshua.moerman/mealy-decompose.
-- It implements the splitting tree algorithm [1] for computing all
-- separating sequences efficiently. This can be used as a characterisation
-- set for the W-method, or as the basis of the HSI-method. (The splitting
-- tree algorithm itself is derived from Hopcroft's minimisation algorithm [2].)
--
-- [1]: Minimal Separating Sequences for All Pairs of States
-- https://doi.org/10.1007/978-3-319-30000-9_14
--
-- [2]: An n log n algorithm for minimizing states in a finite automaton
-- https://doi.org/10.1016/B978-0-12-417750-5.50022-1
module Haal.EquivalenceOracle.HSIMethod (
  HSIMethod,
  mkHSIMethod,
  Block,
  Partition(..),
  InnerNode,
  SplittingTree(..),
  refine,
  splittingTreeMealy,
  stateIdentifierFor,
) where

import Control.Monad.State (gets, modify', evalState, MonadState(..))
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Haal.BlackBox (Automaton(..), Finite, FiniteOrd, accessSequences)
import Haal.EquivalenceOracle (EquivalenceOracle(..))
import Haal.EquivalenceOracle.WMethod (WMethodConfig(..))

-- * HSI-method Equivalence Oracle

-- | The 'HSIMethod' type represents the HSI-method equivalence oracle.
newtype HSIMethod = HSIMethod
  { config :: WMethodConfig }
  deriving (Show, Eq)

-- | Constructor for a 'HSIMethod' value. (Depth must be non-negative.)
mkHSIMethod :: WMethodConfig -> Either String HSIMethod
mkHSIMethod cfg
    | 0 <= wmDepth cfg = Right (HSIMethod cfg)
    | otherwise = Left ("Must be 0 <= wmDepth but got " ++ show cfg)

instance EquivalenceOracle HSIMethod where
  testSuite hsi aut =
    let
      -- covers the states and transitions
      stateCover = computePrefixes (wmDepth . config $ hsi) aut
      (p, st) = splittingTreeMealy aut
      si = stateIdentifierFor p st
      suite = [(reverse px) <> sx | (s, ps) <- Map.toList stateCover, px <- Set.toList ps, sx <- si s]
    in (hsi, suite)

-- * Splitting Tree algorithm

-- ** Basics

-- | A `Block` is a unique identifier for a set of elements.
newtype Block = Block Int
  deriving (Eq, Ord, Show, Enum, Num) via Int

-- | A `Partition` is represented by a finite map of type @s -> `Block`@. Two
-- elements mapped to the same block are equivalent. Note that a permutation
-- of the blocks will not change the partition, but it does change the
-- underlying representation. That is why there is no `Eq` instance.
newtype Partition s = Partition
  { getPartition :: Map.Map s Block }
  deriving Show

-- In the splitting tree, we record the splits we have made during partition
-- refinement. The leafs correspond to the blocks in the partition, and the
-- other nodes will be inner nodes. An inner node is just a number, the
-- associated information is kept in the SplittingTree type.
newtype InnerNode = Node Int
  deriving (Eq, Ord, Read, Show, Enum) via Int

-- Note that this type of tree is given by the "parent relation", so it is not
-- inductively defined. (And we don't need that for partition refinement.)
data SplittingTree s i o = SplittingTree
  { label :: Map.Map InnerNode [i]
  -- ^ The separating word for this node. We use a lazy list for this, because
  --   sharing is important here.
  , innerParent :: Map.Map InnerNode (InnerNode, o)
  -- ^ Tree structure. The node without parent is the root.
  , blockParent :: Map.Map Block (InnerNode, o)
  -- ^ Tree structure of the leaves.
  , size :: Map.Map Block Int
  -- ^ Size of each block of the partition
  }
  deriving (Show)

-- Add size, and perhaps some info about the missing subblock
data Splitter s i o = Splitter
  { split :: Map.Map o [s]
  , leftOut :: o
  , witness :: [i]
  }
  deriving (Show)

-- The data structure used during partition refinement.
data PRState s i o = PRState
  { partition :: Partition s
  , nextBlockId :: Block
  , splittingTree :: SplittingTree s i o
  , nextNodeId :: InnerNode
  }
  deriving (Show)

updatePartition :: (MonadState (PRState s i o) m, Ord s) => s -> Block -> m ()
updatePartition s b = modify' foo
 where
  foo prs = prs{partition = coerce (Map.insert s b) (partition prs)}

updateSize :: (MonadState (PRState s i o) m) => Block -> Int -> m Int
updateSize b n =
  modify' (\prs -> prs{splittingTree = (splittingTree prs){size = Map.insert b n (size (splittingTree prs))}})
    >> return n

genNextBlockId :: (MonadState (PRState s i o) m) => m Block
genNextBlockId = do
  idx <- gets nextBlockId
  modify' (\prs -> prs{nextBlockId = succ (nextBlockId prs)})
  return idx

updateParent :: (MonadState (PRState s i o) m) => Either Block InnerNode -> InnerNode -> o -> m ()
updateParent (Left block) target output = modify' foo
 where
  foo prs = prs{splittingTree = (splittingTree prs){blockParent = Map.insert block (target, output) (blockParent (splittingTree prs))}}
updateParent (Right node) target output = modify' foo
 where
  foo prs = prs{splittingTree = (splittingTree prs){innerParent = Map.insert node (target, output) (innerParent (splittingTree prs))}}

updateLabel :: (MonadState (PRState s i o) m) => InnerNode -> [i] -> m ()
updateLabel node witness = modify' (\prs -> prs{splittingTree = (splittingTree prs){label = Map.insert node witness (label (splittingTree prs))}})

genNextNodeId :: (MonadState (PRState s i o) m) => m InnerNode
genNextNodeId = do
  idx <- gets nextNodeId
  modify' (\prs -> prs{nextNodeId = succ (nextNodeId prs)})
  return idx

cleanupP :: Partition s -> Partition s
cleanupP (Partition m) = Partition p
 where
  p = evalState (Map.traverseWithKey update m) (Map.empty, Block 0)
  update _ v = do
    (m2, n) <- get
    case Map.lookup v m2 of
      Just k -> pure k
      Nothing -> do
        put (Map.insert v n m2, succ n)
        pure n

-- ** Main algorithm

refineWithSplitter :: (MonadState (PRState s i o) m, Ord o, Ord s) => i -> (s -> [s]) -> Splitter s i o -> m [Splitter s i o]
refineWithSplitter action rev splitter = do
  currentPartition <- gets (getPartition . partition)
  currentSplittingTree <- gets splittingTree

  let
    -- For each block in the splitter, we get its predecessors.
    predecessors = Map.map (concatMap rev) (split splitter)

    -- For each block that we found, we are going to create temporary children.
    -- Only when the splitter actually splits the subblock, we change the
    -- partition. We work on list here, because we are going to re-order
    -- the data anyways.
    tempChildsList = [(b, [(o, [s])]) | (o, ls) <- Map.toList predecessors, s <- ls, let b = currentPartition Map.! s]

    -- We need it sorted on the block and the output
    tempChildsMaps = Map.map (Map.fromListWith (++)) . Map.fromListWith (++) $ tempChildsList

    -- Now we need to check the 3-way split:
    -- \* Some blocks have no states which occured, these don't appear.
    -- \* Some blocks have all states move to a single subblock, this is not a
    --   proper split and should be removed.
    -- \* Some blocks have different outputs (a proper split) or states which
    --   moved and states which didn't.
    properSplit b os
      | Map.null os = error "Should not happen"
      | Map.size os >= 2 = True
      | length (head (Map.elems os)) == size currentSplittingTree Map.! b = False
      | otherwise = True

    -- We keep the proper splits only
    tempChildsMaps2 = Map.filterWithKey properSplit tempChildsMaps

    -- Now we can assign new blocks to the newly split states.
    updateSubBlock nNIdx o ls = do
      -- Create a new sub-block
      nBIdx <- genNextBlockId
      -- Set all states to that id
      mapM_ (`updatePartition` nBIdx) ls
      -- And update the tree
      updateParent (Left nBIdx) nNIdx o
      n <- updateSize nBIdx (length ls)
      return (n, ls)

    updateBlock b children = do
      -- Create a new inner node
      nNIdx <- genNextNodeId
      -- Update all sub-blocks
      sizesAndSubblocks <- Map.traverseWithKey (updateSubBlock nNIdx) children

      -- There may be states remaining in b (because we process the smaller
      -- halves). So we need to update its size.
      -- TODO: Do we need to remove b if it is empty?
      let oldSize = size currentSplittingTree Map.! b
          missingSize = oldSize - sum (Map.map fst sizesAndSubblocks)
      _ <- updateSize b missingSize

      -- Also update the "missing block". This means b becomes a child of nNIdx
      -- and nNIdx a child of the current parent of b.
      -- And we update the witness by prepending the action
      let (currentParent, op) = blockParent currentSplittingTree Map.! b
          newWitness = action : (witness splitter)
      updateParent (Right nNIdx) currentParent op
      updateParent (Left b) nNIdx (leftOut splitter)
      updateLabel nNIdx newWitness

      -- Lastly, we make the new splitter. We cannot properly remove the
      -- largest subblock, as we would have to look up its states. I'm not
      -- sure it's worth the effort, so we only do it when we can remove a
      -- subblock.
      if missingSize == 0
        then
          let ls = Map.toList sizesAndSubblocks
              -- TODO: sort(On) is unnecessarily expensive, we only need to
              -- know the biggest...
              ((o1, _) : smallerBlocks) = sortOn (\(_, (n, _)) -> -n) ls
           in return
                Splitter
                  { split = Map.fromList (fmap (\(o, (_, lss)) -> (o, lss)) smallerBlocks)
                  , leftOut = o1
                  , witness = newWitness
                  }
        else
          return
            Splitter
              { split = Map.map snd sizesAndSubblocks
              , leftOut = leftOut splitter
              , witness = newWitness
              }

  Map.elems <$> Map.traverseWithKey updateBlock tempChildsMaps2

refineWithOutput :: (MonadState (PRState s i o) m, Ord o, Ord s) => [i] -> (s -> o) -> m [Splitter s i o]
refineWithOutput witness out = do
  currentPartition <- getPartition <$> gets partition
  currentSplittingTree <- gets splittingTree

  let
    -- Compute all outputs and (existing blocks)
    tempChildsList = [(b, [(o, [s])]) | (s, b) <- Map.toList currentPartition, let o = out s]

    -- Then sort them on blocks and outputs
    tempChildsMaps = Map.map (Map.fromListWith (++)) . Map.fromListWith (++) $ tempChildsList

    -- Only consider actual splits
    tempChildsMaps2 = Map.filter (\children -> Map.size children >= 2) tempChildsMaps

    updateStates nNIdx o ss = do
      -- Create a new sub-block
      nBIdx <- genNextBlockId
      -- Set all states to that id
      mapM_ (`updatePartition` nBIdx) ss
      -- And update the tree
      updateParent (Left nBIdx) nNIdx o
      _ <- updateSize nBIdx (length ss)
      return ()

    updateBlock b children = do
      -- We skip the biggest part, and don't update the blocks.
      let ((o1, biggest) : smaller) = sortOn (\(_, ss) -> negate (length ss)) . Map.toList $ children

      -- For the remaining blocks, we update the partition
      nNIdx <- genNextNodeId
      traverse_ (uncurry (updateStates nNIdx)) smaller

      -- If we are doing the very first split, the nNIdx node does not have a
      -- parent. So we don't have to do updates. Now nNIdx will be the root.
      case Map.lookup b (blockParent currentSplittingTree) of
        Nothing -> return ()
        Just (currentParent, op) -> updateParent (Right nNIdx) currentParent op

      updateLabel nNIdx witness

      -- Remember to update the tree structure for the biggest (skipped) block
      updateParent (Left b) nNIdx o1
      _ <- updateSize b (length biggest)

      -- Return the splitter, not that we already skipped the larger part.
      return
        Splitter
          { split = Map.fromList smaller
          , leftOut = o1
          , witness = witness
          }

  Map.elems <$> Map.traverseWithKey updateBlock tempChildsMaps2

initialPRState :: Ord s => [s] -> PRState s i o
initialPRState ls =
  PRState
    { partition = Partition . Map.fromList $ [(s, Block 0) | s <- ls]
    , nextBlockId = Block 1
    , splittingTree =
        SplittingTree
          { label = Map.empty
          , innerParent = Map.empty
          , blockParent = Map.empty
          , size = Map.singleton (Block 0) (length ls)
          }
    , nextNodeId = Node 0
    }

refineWithAllOutputs :: (MonadState (PRState s i o) m, Ord o, Ord s) => [(i, s -> o)] -> m [Splitter s i o]
refineWithAllOutputs ls = concat <$> traverse (uncurry (refineWithOutput . pure)) ls

refineWithSplitterAllInputs :: (MonadState (PRState s i o) m, Ord o, Ord s) => [(i, s -> [s])] -> Splitter s i o -> m [Splitter s i o]
refineWithSplitterAllInputs ls splitter = concat <$> traverse (\(i, rev) -> refineWithSplitter i rev splitter) ls

-- ** Usage

refine :: (MonadState (PRState s i o) m, Ord o, Ord s) => [(i, s -> o)] -> [(i, s -> [s])] -> m (Partition s, SplittingTree s i o)
refine outputs transitionsReverse = do
  initialQueue <- refineWithAllOutputs outputs

  let
    loop [] = return ()
    loop (splitter : splitters) = do
      newQueue <- refineWithSplitterAllInputs transitionsReverse splitter
      loop (splitters <> newQueue)

  loop initialQueue
  prState <- get
  pure (cleanupP (partition prState), splittingTree prState)

splittingTreeMealy :: (Automaton aut s i o, Finite i, FiniteOrd s, Ord o) => aut s i o -> (Partition s, SplittingTree s i o)
splittingTreeMealy aut =
  let alphabet = [minBound .. maxBound]
      delta s i = transitions aut Map.! (s, i)
      stateOutputs = [(i, \s -> snd (delta s i)) | i <- alphabet]

      reverseTransitionMaps i = Map.fromListWith (++) [(t, [s]) | s <- Set.toList (states aut), let t = fst (delta s i)]
      reverseFuns = [(i, fun) | i <- alphabet, let m = reverseTransitionMaps i, let fun s = Map.findWithDefault [] s m]
  in evalState (refine stateOutputs reverseFuns) (initialPRState (Set.toList (states aut)))

stateIdentifierFor :: Ord s => Partition s -> SplittingTree s i o -> s -> [[i]]
stateIdentifierFor p st state = go firstNode
 where
  firstNode = fst <$> (blockParent st) Map.!? ((getPartition p) Map.! state)
  getParent n = fst <$> (innerParent st) Map.!? n
  go Nothing = []
  go (Just n) = ((label st) Map.! n) : go (getParent n)

-- * Access sequences

-- When extending the access sequences, the HSI method needs
-- to know what state we reach, because the suffix depends
-- on the state. The following function does that.
computePrefixes :: (Automaton aut s i o, FiniteOrd i, FiniteOrd s) => Int -> aut s i o -> Map.Map s (Set.Set [i])
computePrefixes extraStates aut = stateCover
 where
    alphabet = [minBound .. maxBound]
    delta s i = fst $ transitions aut Map.! (s, i)

    -- we keep the sequences in reverse order, because we want to prepend
    -- for extra states. Slightly inconvenient here. Maybe should use `Seq`?
    spanningTree = Map.map (Set.singleton . reverse) $ accessSequences aut
    stateCover = nTimes (extraStates + 1) extendStateCover spanningTree

    nTimes 0 _ = id
    nTimes 1 f = f
    nTimes n f = f . nTimes (n-1) f

    -- A set data structure is used to avoid adding prefixes which are already
    -- visited. This is done per state, as this will result in smaller sets.
    extendStateCover m =
      let moreCover = [(delta s a, Set.singleton (a : px)) | (s, pxs) <- Map.toList m, px <- Set.toList pxs, a <- alphabet]
       in foldr (uncurry (Map.insertWith Set.union)) m moreCover

{- Notes on performance:
~~~~~~~~~~~~~~~~~~~~~~~~

- The above algorithm mimics the "process the smaller half" algorithm by
  Hopcroft. However, this requires a slightly more sophisticated data
  structure (sometimes called a /refinable partition/). This introduces a
  second indirection, and is a bit more bookkeeping. Instead of processing
  the smaller half, we process a half (not necessarily the smallest). When
  lucky, this could be the smallest, and we get the O(n log n) bound. But
  when unlucky, we get the O(n^2) bound.

- Usually these algorithms are implemented using vectors. This would require
  the mealy machine to be encoded with integers. This is possible and would
  make the algorithm run faster. However, I like to have actual state names,
  actual outputs and inputs. This makes it easier to debug and less
  error-prone. (For instance, it is impossible to mix up the types.)

- I do not know whether `cleanupP` is needed. I think it is possible that
  gaps are introduced in the block numbering. This is not a problem for the
  partitions refinement. But it is a problem later on, when I need to know
  the exact number of blocks (for instance when comparing partitions in the
  /partition lattice/). The function `cleanupP` does not really show in the
  profiling results.

-}
