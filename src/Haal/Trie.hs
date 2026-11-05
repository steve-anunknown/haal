{- | A trie keyed by sequences of inputs, storing one output per step along the
path. It backs 'Haal.SUL.Cached.CachedSUL': a membership query is a sequence of
inputs, and the trie lets queries that share a prefix share the nodes along
that prefix too.
-}
module Haal.Trie (
    Trie,
    empty,
    lookup,
    insert,
)
where

import qualified Data.Map.Strict as Map
import Prelude hiding (lookup)

-- | A trie from sequences of @i@ to the @o@ produced at each step along the way.
newtype Trie i o = Trie (Map.Map i (o, Trie i o))
    deriving (Show, Eq)

-- | The empty trie, containing no sequence.
empty :: Trie i o
empty = Trie Map.empty

{- | Look up the outputs produced on a sequence of inputs, if that whole
sequence was previously 'insert'ed (possibly as a prefix of a longer one).
Returns 'Nothing' as soon as the sequence diverges from every path in the trie.
-}
lookup :: (Ord i) => Trie i o -> [i] -> Maybe [o]
lookup _ [] = Just []
lookup (Trie m) (x : xs) = do
    (o, t') <- Map.lookup x m
    os <- lookup t' xs
    pure (o : os)

{- | Record the output produced at each step of a sequence of inputs,
overwriting any output already stored for a shared prefix. Pairing each input
with its output, rather than taking two separate lists, rules out mismatched
lengths by construction.
-}
insert :: (Ord i) => Trie i o -> [(i, o)] -> Trie i o
insert t [] = t
insert (Trie m) ((x, o) : xs) =
    Trie (Map.insert x (o, insert child xs) m)
  where
    child = maybe empty snd (Map.lookup x m)
