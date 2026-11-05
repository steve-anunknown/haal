{- | Tests for 'Haal.Trie': a cache keyed by a whole sequence of inputs that
shares nodes along any common prefix.
-}
module TrieSpec (
    spec,
) where

import qualified Haal.Trie as Trie
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), Gen, Property, forAll, listOf, property, vectorOf, (===))
import Utils (Input (..), Output (..))

-- | A sequence of inputs, each paired with the output produced at that step.
genQuery :: Gen [(Input, Output)]
genQuery = listOf arbitrary

prop_missOnEmptyTrie :: Input -> [Input] -> Property
prop_missOnEmptyTrie x xs =
    Trie.lookup (Trie.empty :: Trie.Trie Input Output) (x : xs) === Nothing

spec :: Spec
spec = do
    describe "lookup" $ do
        it "the empty query always hits, even on an empty trie" $
            Trie.lookup (Trie.empty :: Trie.Trie Input Output) [] `shouldBe` Just []

        it "misses a sequence that was never inserted" $
            property prop_missOnEmptyTrie

    describe "insert" $ do
        it "round-trips: looking up what was just inserted returns it" $
            property $ forAll genQuery $ \ps ->
                Trie.lookup (Trie.insert Trie.empty ps) (map fst ps) === Just (map snd ps)

        it "a second insert of the same sequence overwrites the first" $
            property $ forAll genQuery $ \ps1 -> forAll (vectorOf (length ps1) arbitrary) $ \os2 ->
                let is = map fst ps1
                    t = Trie.insert (Trie.insert Trie.empty ps1) (zip is os2)
                 in Trie.lookup t is === Just os2

        it "inserting the empty sequence leaves the trie unchanged" $
            property $ forAll genQuery $ \ps ->
                let t = Trie.insert Trie.empty ps
                 in Trie.insert t [] === t

    describe "prefix sharing" $ do
        it "a prefix of an inserted sequence is also a cache hit" $ do
            let t = Trie.insert Trie.empty [(A, X), (B, Y)]
            Trie.lookup t [A] `shouldBe` Just [X]
            Trie.lookup t [A, B] `shouldBe` Just [X, Y]

        it "diverging after a shared prefix is a miss" $ do
            let t = Trie.insert Trie.empty [(A, X), (B, Y)]
            Trie.lookup t [A, C] `shouldBe` Nothing

        it "an unrelated first symbol is a miss" $ do
            let t = Trie.insert Trie.empty [(A, X), (B, Y)]
            Trie.lookup t [B] `shouldBe` Nothing
