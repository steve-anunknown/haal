module Agda.BlackBox where 

open import Haskell.Prelude

{-# NO_POSITIVITY_CHECK #-}
record SUL (sul : Set → Set → Set) : Set₂ where 
    field 
        step  : ∀ { i o } → sul i o → i → (sul i o) × o 
        reset : ∀ { i o } → sul i o → sul i o 

open SUL

{-# COMPILE AGDA2HS SUL class #-}
