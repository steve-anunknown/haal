module Agda.BlackBox where 

open import Haskell.Prelude
open import Data.Tree.AVL.Sets renaming (⟨Set⟩ to FinSet)

{-# NO_POSITIVITY_CHECK #-}
record SUL (sul : Set → Set → Set) : Set₂ where 
  field 
    step  : ∀ {i o} → sul i o → i → (sul i o × o)
    reset : ∀ {i o} → sul i o → sul i o

{-# COMPILE AGDA2HS SUL class #-}

open SUL {{...}}  -- open fields from instance

postulate
  inputs : ∀ {sul : Set → Set → Set} {i o} → sul i o → FinSet i
  outputs : ∀ {sul : Set → Set → Set} {i o} → sul i o → FinSet o

{-# COMPILE AGDA2HS inputs #-}
{-# COMPILE AGDA2HS outputs #-}

-- This function uses the SUL instance implicitly
walk' : ∀ {sul : Set → Set → Set} {i o : Set} {{_ : SUL sul}} →
       sul i o → List i → List o → sul i o × List o
walk' system [] outputs = system , reverse outputs
walk' system (input ∷ rest) outputs =
  let system' , output = step system input
  in walk' system (rest) (output ∷ outputs)

{-# COMPILE AGDA2HS walk' #-}

walk : ∀ {sul : Set → Set → Set} {i o : Set} {{_ : SUL sul}} →
       sul i o → List i → sul i o × List o
walk system is = walk' system is []

{-# COMPILE AGDA2HS walk #-}

-- Automaton class, with `current` method only for now
record Automaton
  (aut : Set → Set → Set)
  (st : Set)
  : Set₂ where

  field
    overlap ⦃ super ⦄ : SUL aut
    
    current : ∀ {i o} → aut i o → st
    update  : ∀ {i o} → aut i o → st → aut i o
    γ       : ∀ {i o} → aut i o → (st → i → st) 
    δ       : ∀ {i o} → aut i o → (st → i → o)

open Automaton {{...}}

{-# COMPILE AGDA2HS Automaton class #-}

initial : ∀ {aut : Set → Set → Set} {st i o : Set} {{_ : Automaton aut st}} → aut i o → st
initial = current ∘ reset

{-# COMPILE AGDA2HS initial #-}

-- accessSequences : ∀ {aut : Set → Set → Set} {st i o : Set} {{_ : Automaton aut st}} → aut i o → st → List i
-- accessSequences aut = bfs ((initial aut),[]) ∷ [] (FinSet.singleton (initial aut)) _

