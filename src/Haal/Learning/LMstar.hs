{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fplugin=LiquidHaskell 
                -fplugin-opt=LiquidHaskell:--prune-unsorted 
                -fplugin-opt=LiquidHaskell:--no-termination #-}

-- | This module implements the LM* algorithm for learning Mealy automata.
module Haal.Learning.LMstar (
    lmstar,
    LMstar,
    LMstarConfig (..),
    mkLMstar,
)
where

import Control.Monad (foldM, forM)
import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Haal.Automaton.MealyAutomaton
import Haal.BlackBox
import Haal.Experiment

{-@ ignore otRefinePlus @-}
{-@ ignore makeConsistent @-}
{-@ ignore makeClosed @-}
{-@ ignore mkLMstar @-}
{-@ ignore lmstar @-}

{-@ die :: {v:String | false} -> a @-}
die :: String -> a
die = error

{-@ listIndexLH :: xs:[a] -> {n:Int | 0 <= n && n < len xs} -> a @-}
listIndexLH :: [a] -> Int -> a
listIndexLH (x : _) 0 = x
listIndexLH (_ : xs) n = listIndexLH xs (n - 1)
listIndexLH [] _ = die "impossible: list index out of bounds"

{-@ lastLH :: {xs:[a] | len xs > 0} -> a @-}
lastLH :: [a] -> a
lastLH [] = die "impossible: lastLH called on empty list"
lastLH l = last l

-- | The 'ObservationTable' type is a data type for storing the observation table of the LM* algorithm.

{-@ data ObservationTable i o = ObservationTable
    { prefixSetS  :: Set.Set [i]
    , suffixSetE  :: Set.Set {v:[i] | len v > 0}
    , mappingT    :: Map.Map ([i], [i]) o
    , prefixSetSI :: Set.Set {v:[i] | len v > 0}
    } @-}
data ObservationTable i o = ObservationTable
    { prefixSetS :: Set.Set [i]
    , suffixSetE :: Set.Set [i]
    , mappingT :: Map.Map ([i], [i]) o
    , -- more fields to avoid recomputing
      prefixSetSI :: Set.Set [i]
    }
    deriving (Show)

{-@ assume Set.difference :: forall <p :: a -> Bool>.
     Ord a => Set.Set (a<p>) -> Set.Set a -> Set.Set (a<p>)
@-}
{-@ assume Set.cartesianProduct :: forall <p1 :: a -> Bool, p2 :: b -> Bool>.
     (Ord a, Ord b) => Set.Set (a<p1>) -> Set.Set (b<p2>) -> Set.Set (a<p1>, b<p2>)
@-}

{- | The 'LMstarConfig' type is a configuration type for the LM* algorithm.
It allows the user to choose between the original LM* algorithm and the LM+ algorithm.
-}
data LMstarConfig = Star | Plus

-- | The 'LMstar' type is a wrapper around the 'ObservationTable' type and represents the LM* algorithm.
data LMstar i o = LMstar (ObservationTable i o) | LMplus (ObservationTable i o)

{- | The 'mkLMstar' function creates a new instance of the 'LMstar' type. It holds a dummy value
so that the user does not have to provide an initial observation table.
-}
mkLMstar :: LMstarConfig -> LMstar i o
mkLMstar Star = LMstar (error "this is invisible")
mkLMstar Plus = LMplus (error "this is invisible")

-- | The 'equivalentRows' function checks if two rows in the observation table are equivalent.
equivalentRows :: forall i o. (Ord i, Eq o) => ObservationTable i o -> [i] -> [i] -> Bool
equivalentRows ot r1 r2 = and $ Set.map (\e -> mapping (r1, e) == mapping (r2, e)) em
  where
    mapping = flip Map.lookup (mappingT ot)
    em = suffixSetE ot

{- | The 'initializeOT' function initializes the observation table for the LM* algorithm.
It must be in the 'Experiment' monad to allow queries to the SUL.
-}
initializeOT ::
    forall i o sul m.
    (FiniteOrd i, SUL sul m) =>
    ExperimentT (sul i o) m (ObservationTable i o)
initializeOT = do
    sul <- ask
    let
        alph = List.map (: []) $ Set.toList $ inputs sul
        sm = Set.singleton []
        sm_I = Set.fromList alph
        em = Set.fromList alph
        domain = Set.toList $ (sm `Set.union` sm_I) `Set.cartesianProduct` em
    -- monadic mapping because walk is in m
    tmList <- forM domain $ \(in1, in2) -> do
        sul0 <- lift $ reset sul
        (_, outs) <- lift $ walk sul0 (in1 ++ in2)
        pure ((in1, in2), lastLH outs)

    let tm = Map.fromList tmList

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
    (FiniteOrd i, Eq o) =>
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

-- | The 'lmstar' function implements one iteration of the LM* algorithm.
lmstar ::
    forall sul i o m.
    (SUL sul m, FiniteOrd i, Eq o, Monad m) =>
    LMstar i o ->
    ExperimentT (sul i o) m (LMstar i o, MealyAutomaton StateID i o)
lmstar (LMstar ot) = case otIsClosed ot of
    [] -> case otIsConsistent ot of
        ([], []) -> case makeHypothesis ot of
            Just hyp -> return (LMstar ot, hyp)
            Nothing -> error "LM*: invariant violation — makeHypothesis failed on closed consistent table"
        inc' -> do
            ot' <- makeConsistent ot inc'
            lmstar (LMstar ot')
    inc -> do
        ot' <- makeClosed ot inc
        lmstar (LMstar ot')
lmstar (LMplus ot) = case otIsClosed ot of
    [] -> case makeHypothesis ot of
        Just hyp -> return (LMplus ot, hyp)
        Nothing -> error "LM+: invariant violation — makeHypothesis failed on closed table"
    inc -> do
        ot' <- makeClosed ot inc
        lmstar (LMplus ot')

{- | The 'otIsClosed' function checks if the observation table is closed.
The observation table is closed if every prefix of `prefixSetSI` belongs
to the same equivalence class as some prefix of `prefixSetS`. If the observation 
table is closed, it returns an empty list, whereas if it is not, it returns the 
problematic prefix from `prefixSetSI` that does not have the same equivalence class 
as any prefix from `prefixSetS`.
-}
otIsClosed :: forall i o. (FiniteOrd i, Eq o) => ObservationTable i o -> [i]
otIsClosed ot = Maybe.fromMaybe [] exists
  where
    sm = prefixSetS ot
    sm_I = prefixSetSI ot

    exists = List.find (\x -> not $ any (equivalentRows ot x) sm) sm_I

-- | The 'otIsConsistent' function checks if the observation table is consistent.
otIsConsistent :: forall i o. (FiniteOrd i, Eq o) => ObservationTable i o -> ([i], [i])
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
    forall sul i o m.
    (FiniteOrd i, SUL sul m) =>
    ObservationTable i o ->
    [i] ->
    ExperimentT (sul i o) m (ObservationTable i o)
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

    tm' <- lift $ updateMap tm missing sul
    let ot' = ObservationTable{prefixSetS = sm', suffixSetE = em, mappingT = tm', prefixSetSI = sm_I'}
    return ot'

{- | The 'makeHypothesis' function constructs a Mealy automaton from the observation table. It uses
the default 'StateID' type defined in the 'Experiment' module for representing the automaton states.
Returns 'Nothing' if the observation table is malformed (invariant violated).
-}
makeHypothesis :: forall i o. (FiniteOrd i, Eq o) => ObservationTable i o -> Maybe (MealyAutomaton StateID i o)
makeHypothesis ot = do
    startId <- getStateId []
    let stateInputPairs = [(sid, i) | sid <- [0 .. numStates - 1], i <- alphaList]
    deltaEntries <- mapM buildDeltaEntry stateInputPairs
    lambdaEntries <- mapM buildLambdaEntry stateInputPairs
    let deltaMap = Map.fromList deltaEntries
        lambdaMap = Map.fromList lambdaEntries
        delta' sid i = deltaMap Map.! (sid, i)
        lambda' sid i = lambdaMap Map.! (sid, i)
    return $ mkMealyAutomaton delta' lambda' (Set.fromList [0 .. numStates - 1]) startId
  where
    equivMap = equivalenceClasses ot
    repList = Map.keys equivMap
    numStates = length repList
    repToId = Map.fromList (zip repList [0 ..])
    alphaList = [minBound .. maxBound] :: [i]

    getStateId :: [i] -> Maybe StateID
    getStateId s = List.find (equivalentRows ot s) repList >>= flip Map.lookup repToId

    repAt :: StateID -> [i]
    repAt sid = repList `listIndexLH` sid

    buildDeltaEntry :: (StateID, i) -> Maybe ((StateID, i), StateID)
    buildDeltaEntry (sid, i) = do
        let rep = repAt sid
        target <- getStateId (rep ++ [i])
        return ((sid, i), target)

    buildLambdaEntry :: (StateID, i) -> Maybe ((StateID, i), o)
    buildLambdaEntry (sid, i) = do
        let rep = repAt sid
        o <- Map.lookup (rep, [i]) (mappingT ot)
        return ((sid, i), o)

-- | The 'makeConsistent' function makes the observation table consistent by adding missing prefixes.
makeConsistent ::
    forall i o sul m.
    (FiniteOrd i, SUL sul m) =>
    ObservationTable i o ->
    ([i], [i]) ->
    ExperimentT (sul i o) m (ObservationTable i o)
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
        missing' = map (uncurry (++)) $ Set.toList missing

    outs <- lift $ forM missing' (walk sul)

    let
        outs' = map (lastLH . snd) outs
        tm' = foldr (\((a, b), o) -> Map.insert (a, b) o) tm (zip (Set.toList missing) outs')
    return (ObservationTable{prefixSetS = sm, suffixSetE = em', mappingT = tm', prefixSetSI = sm_I})

-- | The 'makeClosed' function makes the observation table closed by adding missing suffixes.
makeClosed ::
    forall sul i o m.
    (FiniteOrd i, SUL sul m) =>
    ObservationTable i o ->
    [i] ->
    ExperimentT (sul i o) m (ObservationTable i o)
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
    outs <- lift $ forM (Set.toList em) (walk sul)
    let mappings = [((inc ++ [s], e), lastLH (snd o)) | s <- alph, (e, o) <- zip (Set.toList em) outs]
        tm' = List.foldr (uncurry Map.insert) tm mappings
    return (ObservationTable{prefixSetS = sm', suffixSetE = em, mappingT = tm', prefixSetSI = sm_I'})

instance Learner LMstar MealyAutomaton StateID where
    initialize (LMstar _) = do
        LMstar <$> initializeOT
    initialize (LMplus _) = do
        LMplus <$> initializeOT

    refine (LMstar ot) cex = do
        ot' <- otRefineAngluin ot cex
        return (LMstar ot')
    refine (LMplus ot) cex = do
        ot' <- otRefinePlus ot cex
        return (LMplus ot')

    learn (LMstar ot) = lmstar (LMstar ot)
    learn (LMplus ot) = lmstar (LMplus ot)

{- | The 'otRefinePlus' function refines the observation table based on a counterexample, according to the LM+ algorithm,
which is an improvement over Angluin's algorithm.
-}
otRefinePlus ::
    forall sul i o m.
    (FiniteOrd i, SUL sul m) =>
    ObservationTable i o ->
    [i] ->
    ExperimentT (sul i o) m (ObservationTable i o)
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
        -- (_, suffix) = Maybe.fromMaybe (error "failed to update observation table") wrapped
        (_, suffix) = Maybe.fromMaybe ([], []) wrapped
        -- the suffix is the distinguishing suffix. insert all suffixes expect from the empty one
        newSuffixes = em `Set.difference` Set.fromList (init $ List.tails suffix)
        em' = List.foldr Set.insert em newSuffixes
        missing = (sm `Set.union` sm_I) `Set.cartesianProduct` newSuffixes

    tm' <- lift $ updateMap tm missing sul
    return (ObservationTable{prefixSetS = sm, suffixSetE = em', mappingT = tm', prefixSetSI = sm_I})

{-@ updateMap :: (Ord i, SUL sul m i o, Monad m)
              => Map.Map ([i],[i]) o
              -> Set.Set ([i], {v:[i] | len v > 0})
              -> sul i o
              -> m (Map.Map ([i],[i]) o) @-}
updateMap ::
    (Ord i, SUL sul m, Monad m) =>
    Map.Map ([i], [i]) o ->
    Set.Set ([i], [i]) ->
    sul i o ->
    m (Map.Map ([i], [i]) o)
updateMap themap thestuff thesul =
    foldM
        ( \acc (a, b) -> do
            (_, outs) <- walk thesul (a ++ b)
            let o = lastLH outs
            pure (Map.insert (a, b) o acc)
        )
        themap
        thestuff
