module Agda.BlackBox where 

open import Haskell.Prelude

{-# NO_POSITIVITY_CHECK #-}
record SUL (sul : Set → Set → Set) : Set₂ where 
    field 
        step  : ∀ { i o } → sul i o → i → (sul i o) × o 
        reset : ∀ { i o } → sul i o → sul i o 

open SUL

{-# COMPILE AGDA2HS SUL class #-}

helpWalk : ∀ {sul i o} → sul i o → List i → (sul i o) × (List o)
helpWalk system [] os = system , reverse os 
helpWalk system (input ∷ inputs) os = 
    let system' , output = (step system) system input 
    in helpWalk system' inputs (output ∷ outputs)

walk : ∀ {sul i o} → sul i o → List i → (sul i o) × (List o)
walk system inputs = helpWalk system inputs []

