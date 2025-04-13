{-# LANGUAGE ScopedTypeVariables #-}

module Lstar (
    initializeOT,
    lstar,
)
where

import BlackBox
import Control.Monad.Reader
import Data.Data
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Experiment

data ObservationTable i o = ObservationTable
    { prefixSetS :: Set.Set [i]
    , suffixSetE :: Set.Set [i]
    , mappingT :: Map.Map ([i], [i]) [o]
    }

rows :: forall i o. (Ord i, Data i) => ObservationTable i o -> Set.Set [i]
rows ot = sm `Set.union` sm_I
  where
    alph = List.map fromConstr (dataTypeConstrs $ dataTypeOf (undefined :: i)) :: [i]
    sm = prefixSetS ot
    sm_I = Set.fromList [w ++ [a] | w <- Set.toList sm, a <- alph]

equivalentRows :: forall i o. (Ord i, Eq o) => ObservationTable i o -> [i] -> [i] -> Bool
equivalentRows ot r1 r2 = and $ Set.map (\e -> mapping (r1, e) == mapping (r2, e)) em
  where
    mapping = flip Map.lookup (mappingT ot)
    em = suffixSetE ot

columns = suffixSetE

initializeOT ::
    forall i o s sul.
    (Data i, Ord i, SUL sul, Ord s, Bounded s) =>
    sul i o s ->
    ObservationTable i o
initializeOT sul = do
    let alph = List.map ((: []) . fromConstr) (dataTypeConstrs $ dataTypeOf (undefined :: i)) :: [[i]]
        domain = Set.fromList alph `Set.cartesianProduct` Set.fromList alph
        theMapping =
            Map.fromList
                [ ( (in1, in2)
                  , snd $ walk (reset sul) (in1 ++ in2)
                  )
                | (in1, in2) <- Set.toList domain
                ]
     in ( ObservationTable
            { prefixSetS = Set.singleton []
            , suffixSetE = Set.fromList alph
            , mappingT = theMapping
            }
        )

otIsClosed :: forall i o. (Data i, Ord i, Eq o) => ObservationTable i o -> [i]
otIsClosed ot = Maybe.fromMaybe [] exists
  where
    alph = List.map fromConstr (dataTypeConstrs $ dataTypeOf (undefined :: i)) :: [i]
    sm = prefixSetS ot
    sm_I = [w ++ [a] | w <- Set.toList sm, a <- alph]

    exists = List.find (\x -> not $ all (equivalentRows ot x) sm) sm_I

otIsConsistent :: forall i o. (Data i, Ord i, Eq o) => ObservationTable i o -> ([i], [i])
otIsConsistent ot = Maybe.fromMaybe ([], []) condition
  where
    alph = List.map fromConstr (dataTypeConstrs $ dataTypeOf (undefined :: i)) :: [i]
    sm = Set.toList $ prefixSetS ot

    equivalentPairs = [(r1, r2) | r1 <- sm, r2 <- sm, r1 /= r2, equivalentRows ot r1 r2]

    condition =
        List.find
            ( \(a, b) ->
                any
                    (\x -> not (equivalentRows ot (a ++ [x]) (b ++ [x])))
                    alph
            )
            equivalentPairs

makeClosed ::
    forall sul i o s.
    (Ord i, Data i, SUL sul, Ord s) =>
    ObservationTable i o ->
    [i] ->
    sul i o s ->
    ObservationTable i o
makeClosed ot [] _ = ot
makeClosed ot inc sul = ObservationTable{prefixSetS = em, suffixSetE = sm', mappingT = tm'}
  where
    alph = List.map fromConstr (dataTypeConstrs $ dataTypeOf (undefined :: i)) :: [i]
    sm = prefixSetS ot
    em = suffixSetE ot
    tm = mappingT ot
    sm' = inc `Set.insert` sm
    tm' = List.foldr (uncurry Map.insert) tm [((inc ++ [s], e), snd $ walk sul e) | s <- alph, e <- Set.toList em]

lstar ::
    (Automaton aut, SUL sul, Data i, Ord i, Ord s, Bounded s, Eq o) =>
    aut i o s ->
    Experiment (sul i o s) (aut i o s)
lstar aut = do
    sul <- ask
    lstarHelp aut (initializeOT sul)

lstarHelp ::
    (Automaton aut, SUL sul, Data i, Ord i, Ord s, Bounded s, Eq o) =>
    aut i o s ->
    ObservationTable i o ->
    Experiment (sul i o s) (aut i o s)
lstarHelp aut ot = case otIsClosed ot of
    [] -> case otIsConsistent ot of
        ([], []) -> return $ makeHypothesis ot
        inc' -> do
            sul <- ask
            let ot' = makeConsistent ot inc' sul
            lstarHelp aut ot'
    inc -> do
        sul <- ask
        let ot' = makeClosed ot inc sul
        lstarHelp aut ot'

equivalentToRow :: (Ord i, Eq o) => ObservationTable i o -> [i] -> (Set.Set [i], ObservationTable i o)
equivalentToRow ot r = (equivalents, ot')
  where
    sm = prefixSetS ot
    em = suffixSetE ot 
    tm = mappingT ot
    equivalents = Set.filter (equivalentRows ot r) sm
    sm' = sm `Set.difference` equivalents
    ot' = ObservationTable {prefixSetS=sm', suffixSetE=em, mappingT=tm}

equivalenceClasses :: (Ord i, Eq o) => ObservationTable i o -> [[[i]]]
equivalenceClasses ot = go [] (prefixSetS ot)
    where 
        go acc s 
            | Set.null s = reverse acc 
            | otherwise = 
                let (x, rest) = Set.deleteFindMin s 
                    (equivClass, remainder) = Set.partition (equivalentRows ot x) rest 
                in 
                    go ((x : Set.toList equivClass) : acc) remainder


makeHypothesis :: forall aut i o s. (Data i, Ord i, Eq o) => ObservationTable i o -> aut i o s
makeHypothesis ot = error "todo"
  where
    classes = equivalenceClasses ot
    number = length classes


makeConsistent ::
    forall i o s sul.
    (Ord i, SUL sul, Ord s, Data i) =>
    ObservationTable i o ->
    ([i], [i]) ->
    sul i o s ->
    ObservationTable i o
makeConsistent ot ([], []) _ = ot
makeConsistent ot (column, symbol) sul = ObservationTable{prefixSetS = sm, suffixSetE = em', mappingT = tm'}
  where
    alph = List.map fromConstr (dataTypeConstrs $ dataTypeOf (undefined :: i)) :: [i]

    query = symbol ++ column
    -- prefices = [take n query | n <- [1 .. length query]]

    -- only the query itself must be inserted.
    -- the suffixes are already members.
    em = suffixSetE ot
    em' = query `Set.insert` em

    sm = prefixSetS ot
    sm_I = Set.fromList [w ++ [a] | w <- Set.toList sm, a <- alph]

    tm = mappingT ot

    missing = (sm `Set.union` sm_I) `Set.cartesianProduct` em'

    tm' = Set.foldr (\(a, b) -> Map.insert (a, b) (snd $ walk sul (a ++ b))) tm missing
