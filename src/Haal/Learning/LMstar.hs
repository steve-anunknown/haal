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
    LMstarState (..),
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

{-@ die :: {v:String | false} -> a @-}
die :: String -> a
die = error

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

-- | The 'LMstarState' type tracks whether the observation table has been initialized.
data LMstarState i o = Uninit | Init (ObservationTable i o)
    deriving (Show)

{-@ measure isUninit @-}
isUninit :: LMstarState i o -> Bool
isUninit Uninit = True
isUninit _ = False

{-@ measure lmState @-}
lmState :: LMstar i o -> LMstarState i o
lmState (LMstar s) = s
lmState (LMplus s) = s

-- | The 'LMstar' type wraps an 'LMstarState' and represents the LM* algorithm.
data LMstar i o = LMstar (LMstarState i o) | LMplus (LMstarState i o)

-- | The 'mkLMstar' function creates a new uninitialized instance of the 'LMstar' type.

{-@ mkLMstar :: LMstarConfig -> {v:LMstar i o | isUninit (lmState v)} @-}
mkLMstar :: LMstarConfig -> LMstar i o
mkLMstar Star = LMstar Uninit
mkLMstar Plus = LMplus Uninit

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

{-@ equivalenceClasses :: (FiniteOrd i, Eq o) => ObservationTable i o -> Map.Map [i] [[i]] @-}
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
lmstar (LMstar (Init ot)) = case otIsClosed ot of
    [] -> case otIsConsistent ot of
        ([], []) -> case makeHypothesis ot of
            Just hyp -> return (LMstar (Init ot), hyp)
            Nothing -> die "LM*: invariant violation — makeHypothesis failed on closed consistent table"
        inc' -> do
            ot' <- makeConsistent ot inc'
            lmstar (LMstar (Init ot'))
    inc -> do
        ot' <- makeClosed ot inc
        lmstar (LMstar (Init ot'))
lmstar (LMplus (Init ot)) = case otIsClosed ot of
    [] -> case makeHypothesis ot of
        Just hyp -> return (LMplus (Init ot), hyp)
        Nothing -> die "LM+: invariant violation — makeHypothesis failed on closed table"
    inc -> do
        ot' <- makeClosed ot inc
        lmstar (LMplus (Init ot'))
lmstar (LMstar Uninit) = die "lmstar called before initialize"
lmstar (LMplus Uninit) = die "lmstar called before initialize"

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

{- | The 'otIsConsistent' function checks if the observation table is consistent.
Returns @([], [])@ if consistent, otherwise returns @([a], e)@ where @a@ is the
distinguishing letter and @e@ is an existing suffix witnessing the inconsistency,
so that @[a] ++ e@ can be added to E.
-}
otIsConsistent :: forall i o. (FiniteOrd i, Eq o) => ObservationTable i o -> ([i], [i])
otIsConsistent ot = Maybe.fromMaybe ([], []) condition
  where
    alph = [minBound .. maxBound] :: [i]
    sm = Set.toList $ prefixSetS ot
    em = Set.toList $ suffixSetE ot

    equivalentPairs = [(r1, r2) | r1 <- sm, r2 <- sm, r1 /= r2, equivalentRows ot r1 r2]

    condition = do
        (s1, s2) <-
            List.find
                ( \(s1, s2) ->
                    any (\x -> not (equivalentRows ot (s1 ++ [x]) (s2 ++ [x]))) alph
                )
                equivalentPairs
        x <-
            List.find
                (\x -> not (equivalentRows ot (s1 ++ [x]) (s2 ++ [x])))
                alph
        e <-
            List.find
                (\e -> Map.lookup (s1 ++ [x], e) (mappingT ot) /= Map.lookup (s2 ++ [x], e) (mappingT ot))
                em
        return ([x], e)

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

{-@ makeHypothesis :: (FiniteOrd i, Eq o) => ObservationTable i o -> Maybe (MealyAutomaton StateID i o) @-}
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
    idToRep = Map.fromList (zip [0 ..] repList)
    alphaList = [minBound .. maxBound] :: [i]

    getStateId :: [i] -> Maybe StateID
    getStateId s = List.find (equivalentRows ot s) repList >>= flip Map.lookup repToId

    repAt :: StateID -> Maybe [i]
    repAt sid = Map.lookup sid idToRep

    buildDeltaEntry :: (StateID, i) -> Maybe ((StateID, i), StateID)
    buildDeltaEntry (sid, i) = do
        rep <- repAt sid
        target <- getStateId (rep ++ [i])
        return ((sid, i), target)

    buildLambdaEntry :: (StateID, i) -> Maybe ((StateID, i), o)
    buildLambdaEntry (sid, i) = do
        rep <- repAt sid
        o <- Map.lookup (rep, [i]) (mappingT ot)
        return ((sid, i), o)

-- | The 'makeConsistent' function makes the observation table consistent by adding missing prefixes.

{-@ makeConsistent :: (FiniteOrd i, SUL sul m) =>
      ObservationTable i o ->
      ([i], [i]) ->
      ExperimentT (sul i o) m (ObservationTable i o) @-}
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
        em = suffixSetE ot
        em' = query `Set.insert` em
        sm = prefixSetS ot
        sm_I = prefixSetSI ot
        tm = mappingT ot
        missing = (sm `Set.union` sm_I) `Set.cartesianProduct` Set.singleton query
    tm' <- lift $ updateMap tm missing sul
    return (ObservationTable{prefixSetS = sm, suffixSetE = em', mappingT = tm', prefixSetSI = sm_I})

-- | The 'makeClosed' function makes the observation table closed by adding missing suffixes.

{-@ makeClosed :: (FiniteOrd i, SUL sul m) =>
      ObservationTable i o ->
      [i] ->
      ExperimentT (sul i o) m (ObservationTable i o) @-}
makeClosed ::
    forall sul i o m.
    (FiniteOrd i, SUL sul m) =>
    ObservationTable i o ->
    [i] ->
    ExperimentT (sul i o) m (ObservationTable i o)
makeClosed ot [] = return ot
makeClosed ot inc = do
    sul <- ask
    let alph = inputs sul
        sm = prefixSetS ot
        em = suffixSetE ot
        tm = mappingT ot
        sm' = inc `Set.insert` sm
        sm_I' = Set.map (\(w, a) -> w ++ [a]) (sm' `Set.cartesianProduct` alph)
        newPrefixes = Set.map (\a -> inc ++ [a]) alph
        missing = newPrefixes `Set.cartesianProduct` em
    tm' <- lift $ updateMap tm missing sul
    return (ObservationTable{prefixSetS = sm', suffixSetE = em, mappingT = tm', prefixSetSI = sm_I'})

instance Learner LMstar MealyAutomaton StateID where
    initialize (LMstar _) = do
        LMstar . Init <$> initializeOT
    initialize (LMplus _) = do
        LMplus . Init <$> initializeOT

    refine (LMstar (Init ot)) cex = do
        ot' <- otRefineAngluin ot cex
        return (LMstar (Init ot'))
    refine (LMplus (Init ot)) cex = do
        ot' <- otRefinePlus ot cex
        return (LMplus (Init ot'))
    refine (LMstar Uninit) _ = die "refine called before initialize"
    refine (LMplus Uninit) _ = die "refine called before initialize"

    learn (LMstar (Init ot)) = lmstar (LMstar (Init ot))
    learn (LMplus (Init ot)) = lmstar (LMplus (Init ot))
    learn (LMstar Uninit) = die "learn called before initialize"
    learn (LMplus Uninit) = die "learn called before initialize"

{- | The 'otRefinePlus' function refines the observation table based on a counterexample, according to the LM+ algorithm,
which is an improvement over Angluin's algorithm.
-}
{-@ otRefinePlus :: (FiniteOrd i, SUL sul m) => 
      ObservationTable i o -> 
      [i] ->
      ExperimentT (sul i o) m (ObservationTable i o) @-}
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
    case wrapped of
        Nothing -> return ot
        Just (_, suffix) -> do
            let -- the suffix is the distinguishing suffix. insert all non-empty tails not already in E
                newSuffixes = Set.fromList (init $ List.tails suffix) `Set.difference` em
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
