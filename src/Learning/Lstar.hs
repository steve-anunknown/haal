{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module implements the L* algorithm for learning Mealy automata.
module Learning.Lstar (
    lstar,
    Lstar (..),
    LstarConfig (..),
    ObservationTable (..),
    initializeOT,
    mkLstar,
)
where

import Automaton.MealyAutomaton
import BlackBox
import Control.Monad.Reader
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Experiment

-- | The 'ObservationTable' type is a data type for storing the observation table of the L* algorithm.
data ObservationTable i o = ObservationTable
    { prefixSetS :: Set.Set [i]
    , suffixSetE :: Set.Set [i]
    , mappingT :: Map.Map ([i], [i]) o
    , -- more fields to avoid recomputing
      prefixSetSI :: Set.Set [i]
    }
    deriving (Show)

{- | The 'LstarConfig' type is a configuration type for the L* algorithm.
It allows the user to choose between the original L* algorithm and the L+ algorithm.
-}
data LstarConfig = Star | Plus

-- | The 'Lstar' type is a wrapper around the 'ObservationTable' type and represents the L* algorithm.
data Lstar i o = Lstar (ObservationTable i o) | Lplus (ObservationTable i o)

{- | The 'mkLstar' function creates a new instance of the 'Lstar' type. It holds a dummy value
so that the user does not have to provide an initial observation table.
-}
mkLstar :: LstarConfig -> Lstar i o
mkLstar Star = Lstar (error "this is invisible")
mkLstar Plus = Lplus (error "this is invisible")

-- | The 'equivalentRows' function checks if two rows in the observation table are equivalent.
equivalentRows :: forall i o. (Ord i, Eq o) => ObservationTable i o -> [i] -> [i] -> Bool
equivalentRows ot r1 r2 = and $ Set.map (\e -> mapping (r1, e) == mapping (r2, e)) em
  where
    mapping = flip Map.lookup (mappingT ot)
    em = suffixSetE ot

{- | The 'initializeOT' function initializes the observation table for the L* algorithm.
It must be in the 'Experiment' monad to allow queries to the SUL.
-}
initializeOT ::
    forall i o sul.
    (Bounded i, Enum i, Ord i, SUL sul) =>
    Experiment (sul i o) (ObservationTable i o)
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

-- | The 'equivalenceClasses' function computes the equivalence classes of the observation table.
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

-- | The 'lstar' function implements one iteration of the L* algorithm.
lstar ::
    (SUL sul, Bounded i, Enum i, Ord i, Eq o) =>
    Lstar i o ->
    Experiment (sul i o) (Lstar i o, MealyAutomaton StateID i o)
lstar (Lstar ot) = case otIsClosed ot of
    [] -> case otIsConsistent ot of
        ([], []) -> return (Lstar ot, makeHypothesis ot)
        inc' -> do
            ot' <- makeConsistent ot inc'
            lstar (Lstar ot')
    inc -> do
        ot' <- makeClosed ot inc
        lstar (Lstar ot')
lstar (Lplus ot) = case otIsClosed ot of
    [] -> return (Lplus ot, makeHypothesis ot)
    inc -> do
        ot' <- makeClosed ot inc
        lstar (Lplus ot')

-- | The 'otIsClosed' function checks if the observation table is closed.
otIsClosed :: forall i o. (Bounded i, Enum i, Ord i, Eq o) => ObservationTable i o -> [i]
otIsClosed ot = Maybe.fromMaybe [] exists
  where
    sm = prefixSetS ot
    sm_I = prefixSetSI ot

    exists = List.find (\x -> not $ any (equivalentRows ot x) sm) sm_I

-- | The 'otIsConsistent' function checks if the observation table is consistent.
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

-- | The 'otRefineAngluin' function refines the observation table based on a counterexample, according to Angluin's algorithm.
otRefineAngluin ::
    forall sul i o.
    (Ord i, SUL sul, Bounded i, Enum i) =>
    ObservationTable i o ->
    [i] ->
    Experiment (sul i o) (ObservationTable i o)
otRefineAngluin ot [] = return ot
otRefineAngluin ot cex = do
    sul <- ask
    let
        sm = prefixSetS ot
        em = suffixSetE ot
        tm = mappingT ot
        -- insert all prefixes of the counterexample
        sm' = List.foldr Set.insert sm [take n cex | n <- [1 .. length cex]]
        sm_I' = Set.fromList [w ++ [a] | w <- Set.toList sm', a <- Set.toList $ inputs sul]
        missing = (sm' `Set.union` sm_I') `Set.cartesianProduct` em

        tm' = updateMap tm missing sul
        ot' = ObservationTable{prefixSetS = sm', suffixSetE = em, mappingT = tm', prefixSetSI = sm_I'}
    return ot'

{- | The 'makeHypothesis' function constructs a Mealy automaton from the observation table. It uses
the default 'StateID' type defined in the 'Experiment' module for representing the automaton states.
-}
makeHypothesis :: forall i o. (Ord i, Eq o, Bounded i, Enum i) => ObservationTable i o -> MealyAutomaton StateID i o
makeHypothesis ot = mkMealyAutomaton delta' lambda' (Set.fromList [0 .. length repList - 1]) initial
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
        let rep = repList !! sid
         in getStateId (rep ++ [i])

    lambda' :: StateID -> i -> o
    lambda' sid i =
        let rep = repList !! sid
         in mappingT ot Map.! (rep, [i])

    initial = getStateId []

-- | The 'makeConsistent' function makes the observation table consistent by adding missing prefixes.
makeConsistent ::
    forall i o sul.
    (Ord i, SUL sul, Bounded i, Enum i) =>
    ObservationTable i o ->
    ([i], [i]) ->
    Experiment (sul i o) (ObservationTable i o)
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

-- | The 'makeClosed' function makes the observation table closed by adding missing suffixes.
makeClosed ::
    forall sul i o.
    (Ord i, Bounded i, Enum i, SUL sul) =>
    ObservationTable i o ->
    [i] ->
    Experiment (sul i o) (ObservationTable i o)
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

instance Learner Lstar (MealyAutomaton StateID) where
    initialize (Lstar _) = do
        Lstar <$> initializeOT
    initialize (Lplus _) = do
        Lplus <$> initializeOT

    refine (Lstar ot) cex = do
        ot' <- otRefineAngluin ot cex
        return (Lstar ot')
    refine (Lplus ot) cex = do
        ot' <- otRefinePlus ot cex
        return (Lplus ot')

    learn (Lstar ot) = lstar (Lstar ot)
    learn (Lplus ot) = lstar (Lplus ot)

{- | The 'otRefinePlus' function refines the observation table based on a counterexample, according to the L+ algorithm,
which is an improvement over Angluin's algorithm.
-}
otRefinePlus ::
    forall sul i o.
    (Ord i, SUL sul, Bounded i, Enum i) =>
    ObservationTable i o ->
    [i] ->
    Experiment (sul i o) (ObservationTable i o)
otRefinePlus ot [] = return ot
otRefinePlus ot cex = do
    sul <- ask
    let sm = prefixSetS ot
        em = suffixSetE ot
        tm = mappingT ot
        sm_I = prefixSetSI ot
        -- look for the longest prefix of the counterexample
        -- that is in sm U sm_I
        prefixes = List.inits cex
        suffixes = List.tails cex
        pairs = List.reverse $ List.zip prefixes suffixes
        wrapped = List.find (\x -> Set.member (fst x) sm || Set.member (fst x) sm_I) pairs
        (_, suffix) = Maybe.fromMaybe (error "failed to update observation table") wrapped
        -- the suffix is the distinguishing suffix. insert all suffixes expect from the empty one
        newSuffixes = em `Set.difference` Set.fromList (init $ List.tails suffix)
        em' = List.foldr Set.insert em newSuffixes
        missing = (sm `Set.union` sm_I) `Set.cartesianProduct` newSuffixes
        tm' = updateMap tm missing sul
    return (ObservationTable{prefixSetS = sm, suffixSetE = em', mappingT = tm', prefixSetSI = sm_I})

updateMap :: (Ord i, SUL sul) => Map.Map ([i], [i]) a -> Set.Set ([i], [i]) -> sul i a -> Map.Map ([i], [i]) a
updateMap themap thestuff thesul = Set.foldr (\(a, b) -> Map.insert (a, b) (last $ snd $ walk thesul (a ++ b))) themap thestuff
