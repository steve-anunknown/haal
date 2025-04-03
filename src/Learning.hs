{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Learning (
    mkObservationTable,
)
where

import qualified Data.Data as Data
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (replicateM)

data ObservationTable input output = ObservationTable
    { prefixClosedS :: Set.Set [input]
    , suffixClosedE :: Set.Set [input]
    , mappingT :: Map.Map [input] [output]
    }

-- | This is not exactly correct. This does more than just create a prefix closed set.
-- It is the cartesian product up to dimension 'len', which is different. It is different 
-- because the learning algorithm does not query for every input sequence up to a certain length. 
-- It may require less prompts. What is actually being done is that, for every prompt of the learning algorithm 
-- all of the prompt's prefixes are also inserted. So, for example, if the learning algorithm prompts the system 
-- with [A, B, C, D] -> [[A], [A,B], [A,B,C], [A,B,C,D]].
mkPrefixClosedS :: forall i. (Data.Data i, Ord i) => Int -> Set.Set [i]
mkPrefixClosedS len = Set.fromList (concatMap (`replicateM` justSymbols) [0..len])
  where
    constructors = Data.dataTypeConstrs $ Data.dataTypeOf (undefined :: i)
    justSymbols = List.map Data.fromConstr constructors :: [i]


-- if i could generate a prefix closed type of input, for example
-- data Input = A | B | C
-- prefixClose int type : prefixClose 2 Input -> data InputClosed = AA | AB | AC | BA | BB | BC | CA | CB | CC | A | B | C
-- then the mapping could become a function that could be checked for completeness
-- then again, the idea of replacing a mapping with a function is just a convenience for the user
-- nothing stops me from programmatically making sure that no input is left out. i would have to
-- program the same function to produce the data type anyway; the same mistake could be made following either approach.

mkObservationTable :: forall a i o s. (Data.Data i) => a i o s -> ObservationTable i o
mkObservationTable _ =
    let initialS = Set.singleton []
        constrsI = Data.dataTypeConstrs $ Data.dataTypeOf (undefined :: i)
        initialE = Set.singleton $ List.map Data.fromConstr constrsI
        initialT = Map.empty
     in ObservationTable
            { prefixClosedS = initialS
            , suffixClosedE = initialE
            , mappingT = initialT
            }
