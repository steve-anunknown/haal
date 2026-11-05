{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | A caching wrapper around any SUL. 'CachedSUL' remembers every query sent
through it and the outputs it got back, keyed by the query's inputs, in a
'Haal.Trie.Trie'. The next time the exact same sequence of inputs (or a prefix
of a previously seen one) is queried, the answer comes from the cache instead
of the underlying SUL, and the query is counted as saved.

'CachedSUL' keeps no state of its own: 'Haal.BlackBox.query' can't hand an
updated SUL back to its caller, so the cache has nowhere to live inside the
returned value. Instead it is threaded explicitly through a
'Control.Monad.State.StateT' layer wrapped around the underlying SUL's monad,
the same way 'Haal.Experiment.measuredExperiment' threads statistics through
'Haal.Experiment.Lifted'. Run one with 'Haal.Experiment.cachedExperiment'.

This only works now because @i@\/@o@ are parameters of the 'Haal.BlackBox.SUL'
class itself, rather than varying per method call: the instance below pins
@m@ to @StateT (CacheState i o) m0@ for the *same* @i@\/@o@ that the rest of
the instance is defined at, instead of a fresh pair chosen independently by
each caller. Before that change, an earlier attempt at this exact design
didn't type-check for exactly that reason, and the cache had to live inside
the 'CachedSUL' value via an 'Data.IORef.IORef' instead, restricted to 'IO'.

Caching only applies to 'Haal.BlackBox.query': 'Haal.BlackBox.step' and
'Haal.BlackBox.reset' always go to the underlying SUL, since the cache only
ever sees whole, reset-to-end queries.

This trusts the underlying SUL to be deterministic, i.e. that querying the
same inputs twice gives the same outputs; a later version may want to check
that instead of assuming it.
-}
module Haal.SUL.Cached (
    CachedSUL (..),
    CacheState (..),
    emptyCacheState,
)
where

import Control.Monad.State (MonadTrans (lift), StateT, get, put)
import Haal.BlackBox (SUL (..))
import qualified Haal.Trie as Trie

-- | A SUL wrapped so that caching can be threaded through a 'StateT' around it.
newtype CachedSUL sul i o = CachedSUL (sul i o)

{- | The cache threaded alongside a 'CachedSUL': every query answered so far,
and how many further queries it has saved.
-}
data CacheState i o = CacheState
    { cacheTrie :: Trie.Trie i o
    , queriesSaved :: Int
    }

-- | A fresh, empty cache, having saved no queries yet.
emptyCacheState :: CacheState i o
emptyCacheState = CacheState Trie.empty 0

instance (SUL sul i o m) => SUL (CachedSUL sul) i o (StateT (CacheState i o) m) where
    step (CachedSUL sul) i = do
        (sul', o) <- lift (step sul i)
        pure (CachedSUL sul', o)

    reset (CachedSUL sul) = CachedSUL <$> lift (reset sul)

    query (CachedSUL sul) is = do
        st <- get
        case Trie.lookup (cacheTrie st) is of
            Just os -> do
                put st{queriesSaved = queriesSaved st + 1}
                pure os
            Nothing -> do
                os <- lift (query sul is)
                put st{cacheTrie = Trie.insert (cacheTrie st) (zip is os)}
                pure os
