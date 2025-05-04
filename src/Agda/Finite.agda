module Agda.Finite where

open import Haskell.Prelude

-- Combine them into Finite
record Finite (a : Set) : Set₁ where
  field
    enum   : Enum a
    bounds : Bounded a

{-# COMPILE AGDA2HS Finite class #-}
