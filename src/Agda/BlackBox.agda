module Agda.BlackBox where 

open import Haskell.Prelude
open import Relation.Nullary using (Dec; yes; no)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; decSetoid)

{-# NO_POSITIVITY_CHECK #-}
record SUL (sul : Set → Set → Set) : Set₂ where 
  field 
    step  : ∀ {i o} → sul i o → i → (sul i o × o)
    reset : ∀ {i o} → sul i o → sul i o

{-# COMPILE AGDA2HS SUL class #-}

open SUL {{...}}  -- open fields from instance

-- This function uses the SUL instance implicitly
helpwalk : ∀ {sul : Set → Set → Set} {i o : Set} {{_ : SUL sul}} →
       sul i o → List i → List o → sul i o × List o
helpwalk system [] outputs = system , reverse outputs
helpwalk system (input ∷ rest) outputs =
  let system' , output = step system input
  in helpwalk system (rest) (output ∷ outputs)

{-# COMPILE AGDA2HS helpwalk #-}

walk : ∀ {sul : Set → Set → Set} {i o : Set} {{_ : SUL sul}} →
       sul i o → List i → sul i o × List o
walk system inputs = helpwalk system inputs []

{-# COMPILE AGDA2HS walk #-}

-- Automaton class, with `current` method only for now
record Automaton (aut : Set → Set → Set → Set) : Set₃ where
  field
    overlap ⦃ super ⦄ : ∀ {s} → SUL (aut s)
    current : ∀ {s i o} → aut s i o → s

open Automaton {{...}}

{-# COMPILE AGDA2HS Automaton class #-}

initial : ∀ {aut : Set → Set → Set → Set} {s i o : Set} {{_ : Automaton aut}} → aut s i o → s
initial = current ∘ reset

{-# COMPILE AGDA2HS initial #-}


