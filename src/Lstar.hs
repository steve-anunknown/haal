{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Lstar
(
    lstar
)
where

import BlackBox
import Control.Monad.Reader
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Experiment
import MealyAutomaton (MealyAutomaton, mkMealyAutomaton)

data ObservationTable i o = ObservationTable
    { prefixSetS :: Set.Set [i]
    , suffixSetE :: Set.Set [i]
    , mappingT :: Map.Map ([i], [i]) o
    , -- more fields to avoid recomputing
      prefixSetSI :: Set.Set [i]
    }
    deriving (Show)

type StateID = Word

-- rows :: forall i o. (Ord i, Data i) => ObservationTable i o -> Set.Set [i]
-- rows ot = sm `Set.union` sm_I
--   where
--     alph = List.map fromConstr (dataTypeConstrs $ dataTypeOf (undefined :: i)) :: [i]
--     sm = prefixSetS ot
--     sm_I = Set.fromList [w ++ [a] | w <- Set.toList sm, a <- alph]

equivalentRows :: forall i o. (Ord i, Eq o) => ObservationTable i o -> [i] -> [i] -> Bool
equivalentRows ot r1 r2 = and $ Set.map (\e -> mapping (r1, e) == mapping (r2, e)) em
  where
    mapping = flip Map.lookup (mappingT ot)
    em = suffixSetE ot

-- columns = suffixSetE

initializeOT ::
    forall i o s sul.
    (Bounded i, Enum i, Ord i, SUL sul, Ord s, Bounded s) =>
    Experiment (sul i o s) (ObservationTable i o)
initializeOT = do
    sul <- ask
    let alph = List.map (: []) $ Set.toList $ inputs sul
        sm = Set.singleton []
        sm_I = Set.fromList alph
        em = Set.fromList alph
        domain = (sm `Set.union` sm_I) `Set.cartesianProduct` em
        tm =
            Map.fromList
                [ ( (in1, in2)
                  , last $ snd $ walk (reset sul) (in1 ++ in2)
                  )
                | (in1, in2) <- Set.toList domain
                ]
    return
        ( ObservationTable
            { prefixSetS = sm
            , suffixSetE = em
            , mappingT = tm
            , prefixSetSI = sm_I
            }
        )

-- equivalentToRow :: (Ord i, Eq o) => ObservationTable i o -> [i] -> (Set.Set [i], ObservationTable i o)
-- equivalentToRow ot r = (equivalents, ot')
--   where
--     sm = prefixSetS ot
--     em = suffixSetE ot
--     tm = mappingT ot
--     equivalents = Set.filter (equivalentRows ot r) sm
--     sm' = sm `Set.difference` equivalents
--     ot' = ObservationTable{prefixSetS = sm', suffixSetE = em, mappingT = tm}

equivalenceClasses ::
    forall i o.
    (Ord i, Eq o, Bounded i, Enum i) =>
    ObservationTable i o ->
    Map.Map [i] [[i]]
equivalenceClasses ot = go Map.empty (sm `Set.union` sm_I)
  where
    sm = prefixSetS ot
    sm_I = prefixSetSI ot
    go acc s
        | Set.null s = acc
        | otherwise =
            let (x, rest) = Set.deleteFindMin s
                (equivClass, remainder) = Set.partition (equivalentRows ot x) rest
                classMembers = x : Set.toList equivClass
             in go (Map.insert x classMembers acc) remainder

lstar ::
    (SUL sul, Bounded i, Enum i, Ord i, Ord s, Bounded s, Eq o) =>
    Experiment (sul i o s) (MealyAutomaton i o StateID)
lstar = do
    ot <- initializeOT
    lstarHelp ot

lstarHelp ::
    (SUL sul, Bounded i, Enum i, Ord i, Ord s, Bounded s, Eq o) =>
    ObservationTable i o ->
    Experiment (sul i o s) (MealyAutomaton i o StateID)
lstarHelp ot = case otIsClosed ot of
    [] -> case otIsConsistent ot of
        ([], []) -> return $ makeHypothesis ot
        inc' -> do
            ot' <- makeConsistent ot inc'
            lstarHelp ot'
    inc -> do
        ot' <- makeClosed ot inc
        lstarHelp ot'

otIsClosed :: forall i o. (Bounded i, Enum i, Ord i, Eq o) => ObservationTable i o -> [i]
otIsClosed ot = Maybe.fromMaybe [] exists
  where
    sm = prefixSetS ot
    sm_I = prefixSetSI ot

    exists = List.find (\x -> not $ any (equivalentRows ot x) sm) sm_I

otIsConsistent :: forall i o. (Bounded i, Enum i, Ord i, Eq o) => ObservationTable i o -> ([i], [i])
otIsConsistent ot = Maybe.fromMaybe ([], []) condition
  where
    alph = [minBound .. maxBound] :: [i]
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

-- default to Int for the state type and the user can provide a mapping to whatever
-- type they want for the state.
makeHypothesis :: forall i o. (Ord i, Eq o, Bounded i, Enum i) => ObservationTable i o -> MealyAutomaton i o StateID
makeHypothesis ot = mkMealyAutomaton delta' lambda' initial
  where
    -- Equivalence classes: Map from representative prefix to class members
    equivMap :: Map.Map [i] [[i]]
    equivMap = equivalenceClasses ot

    -- Assign an integer ID to each class representative
    repList :: [[i]]
    repList = Map.keys equivMap

    repToId :: Map.Map [i] StateID
    repToId = Map.fromList (zip repList [0 ..])

    -- Helper: get the ID for the class a string belongs to
    getStateId :: [i] -> StateID
    getStateId s =
        case List.find (\rep -> equivalentRows ot rep s) repList of
            Just rep -> repToId Map.! rep
            Nothing -> error "No equivalent class found for string!"

    delta' :: StateID -> i -> StateID
    delta' sid i =
        let rep = repList !! fromIntegral sid
         in getStateId (rep ++ [i])

    lambda' :: StateID -> i -> o
    lambda' sid i =
        let rep = repList !! fromIntegral sid
         in mappingT ot Map.! (rep, [i])

    initial = getStateId []

makeConsistent ::
    forall i o s sul.
    (Ord i, SUL sul, Ord s, Bounded i, Enum i) =>
    ObservationTable i o ->
    ([i], [i]) ->
    Experiment (sul i o s) (ObservationTable i o)
makeConsistent ot ([], []) = return ot
makeConsistent ot (column, symbol) = do
    sul <- ask
    let
        query = symbol ++ column
        -- prefices = [take n query | n <- [1 .. length query]]

        -- only the query itself must be inserted.
        -- the suffixes are already members.
        em = suffixSetE ot
        em' = query `Set.insert` em

        sm = prefixSetS ot
        sm_I = prefixSetSI ot

        tm = mappingT ot

        missing = (sm `Set.union` sm_I) `Set.cartesianProduct` em'

        tm' = Set.foldr (\(a, b) -> Map.insert (a, b) (last $ snd $ walk sul (a ++ b))) tm missing
    return (ObservationTable{prefixSetS = sm, suffixSetE = em', mappingT = tm', prefixSetSI = sm_I})

makeClosed ::
    forall sul i o s.
    (Ord i, Bounded i, Enum i, SUL sul, Ord s) =>
    ObservationTable i o ->
    [i] ->
    Experiment (sul i o s) (ObservationTable i o)
makeClosed ot [] = return ot
makeClosed ot inc = do
    sul <- ask
    let
        alph = Set.toList $ inputs sul
        sm = prefixSetS ot
        em = suffixSetE ot
        tm = mappingT ot
        sm' = inc `Set.insert` sm
        sm_I' = Set.fromList [w ++ [a] | w <- Set.toList sm', a <- alph]
        tm' = List.foldr (uncurry Map.insert) tm [((inc ++ [s], e), last $ snd $ walk sul e) | s <- alph, e <- Set.toList em]
    return (ObservationTable{prefixSetS = sm', suffixSetE = em, mappingT = tm', prefixSetSI = sm_I'})
