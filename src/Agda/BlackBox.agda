module Agda.BlackBox where 

open import Haskell.Prelude
open import Relation.Binary.Bundles using (StrictTotalOrder)
open import Data.Tree.AVL.Map using (Map)

{-# NO_POSITIVITY_CHECK #-}
record SUL (sul : Set → Set → Set) : Set₂ where 
  field 
    step  : ∀ {i o} → sul i o → i → (sul i o × o)
    reset : ∀ {i o} → sul i o → sul i o

{-# COMPILE AGDA2HS SUL class #-}

open SUL {{...}}  -- open fields from instance

postulate
  inputs : ∀ {sul : Set → Set → Set} {i o} {{ _ : Enum i }} {{ _ : Bounded i }} → sul i o → List i
  outputs : ∀ {sul : Set → Set → Set} {i o} {{ _ : Enum o }} {{ _ : Bounded o }} → sul i o → List o

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
record Automaton (aut : Set → Set → Set) (st : Set) : Set₂ where
  field
    overlap ⦃ super ⦄ : SUL (aut)
    current : ∀ {i o} → aut i o → st
    update  : ∀ {i o} → aut i o → st → aut i o

open Automaton {{...}}

{-# COMPILE AGDA2HS Automaton class #-}

initial : ∀ {aut : Set → Set → Set} {st i o : Set} {{_ : Automaton aut st}} → aut i o → st
initial = current ∘ reset

{-# COMPILE AGDA2HS initial #-}

-- accessSequences : ∀ {aut : Set → Set → Set} {i o : Set} {st}
--                 {{_ : Automaton aut st}} 
--                 {{_ : StrictTotalOrder st }} 
--                 {{_ : Enum i}}
--                 {{_ : Bounded i}} → 
--                 aut i o → Map st (List i)
-- accessSequences = ⊤

