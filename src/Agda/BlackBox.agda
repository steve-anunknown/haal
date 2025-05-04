module Agda.BlackBox where 

open import Haskell.Prelude

{-# NO_POSITIVITY_CHECK #-}
record SUL (i o : Set) : Set where 
    field 
        step : SUL i o → i → (SUL i o) × o 
        reset : SUL i o → SUL i o 

open SUL

{-# COMPILE AGDA2HS SUL class #-}

mapAccumL : ∀ {sul i o} → (sul → i → sul × o) → sul → List i → sul × (List o)  
mapAccumL f acc []        = acc , []
mapAccumL f acc (x ∷ xs) =
    let acc' , y = f acc x
        acc'' , ys = mapAccumL f acc xs
    in acc'' , (y ∷ ys)

walk : ∀ {i o} → (SUL i o) → List i → (SUL i o) × (List o)
walk sul is = mapAccumL (step sul) sul is
