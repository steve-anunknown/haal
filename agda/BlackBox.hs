module agda.BlackBox where

class Mealy s i o where
    q :: [s]
    q₀ :: s
    δ :: s -> i -> s
    γ :: s -> i -> o

