module Agda.Finite where

class Finite a where
    enum :: Enum a
    bounds :: Bounded a

