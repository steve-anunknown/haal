{-# OPTIONS_GHC -fplugin=LiquidHaskellBoot #-}
module Data.Bits_LHAssumptions where

{-@
//  TODO: cannot use this because `Bits` is not a `Num`
//  Data.Bits.shiftR :: (Data.Bits.Bits a) => x:a -> d:Nat 
//                   -> {v:a | ((d=1) => (x <= 2*v + 1 && 2*v <= x)) }
@-}
