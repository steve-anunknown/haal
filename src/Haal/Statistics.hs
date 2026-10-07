{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Haal.Statistics (
    Counts (..),
    Counted (..),
    Phase (..),
    Hook (..),
    noHooks,
    initialCounts,
)
where

import Control.Monad.State (MonadTrans (lift), StateT, modify')
import Haal.BlackBox (SUL (..))

-- Data type that holds basic experiment stats. Important to make it strict
-- because a learning experiment typically involves a great number of queries.
data Counts = Counts {queries :: !Int, symbols :: !Int} deriving (Show, Eq)

initialCounts :: Counts
initialCounts = Counts 0 0

-- Data type that wraps a SUL
newtype Counted sul i o = Counted (sul i o)

-- If `sul m` is an instance of `SUL` then `(Counted sul) (StateT Counts m)` is
-- an instance of SUL.
instance (SUL sul m) => SUL (Counted sul) (StateT Counts m) where
    step :: Counted sul i o -> i -> StateT Counts m (Counted sul i o, o)
    step (Counted sul) input = do
        modify' (\c -> c{symbols = symbols c + 1})
        (sul', output) <- lift (step sul input)
        return (Counted sul', output)
    reset :: Counted sul i o -> StateT Counts m (Counted sul i o)
    reset (Counted sul) = do
        modify' (\c -> c{queries = queries c + 1})
        Counted <$> lift (reset sul)
    query (Counted sul) inputs = do
        modify' (\c -> c{queries = queries c + 1, symbols = length inputs + symbols c})
        lift (query sul inputs)

-- Data type for the possible phases of a learning experiment
data Phase = Learning | Testing deriving (Show, Eq)

-- Data type to express hooks that an experiment can be run with
data Hook m aut i o = Hook
    { onPhase :: Phase -> m ()
    , onHypothesis :: aut Int i o -> m ()
    , onCounterexample :: [i] -> m ()
    }

-- Empty hooks value
noHooks :: (Applicative m) => Hook m aut i o
noHooks = Hook{onPhase = \_ -> pure (), onHypothesis = \_ -> pure (), onCounterexample = \_ -> pure ()}
