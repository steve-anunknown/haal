module LstarTest where

import MealyAutomaton (MealyAutomaton, mkMealyAutomaton)

data In = A | B deriving (Show, Eq, Ord, Bounded, Enum)
data Out = X | Y deriving (Show, Eq, Ord, Bounded, Enum)
data St = S0 | S1 | S2 deriving (Show, Eq, Ord, Bounded, Enum)

lambda :: St -> In -> Out
lambda S0 A = X
lambda S0 B = Y
lambda S1 _ = X
lambda S2 _ = Y

delta :: St -> In -> St
delta S0 _ = S1
delta S1 _ = S2
delta S2 _ = S0

myMealy :: MealyAutomaton In Out St
myMealy = mkMealyAutomaton delta lambda S0

-- >>> ot = initializeOT myMealy
-- >>> ot
-- >>> otIsClosed ot
-- >>> ot' = makeClosed ot [A] myMealy
-- >>> ot'
-- >>> otIsClosed ot'
-- >>> otIsConsistent ot'
-- >>> equivalenceClasses ot'
-- ObservationTable {prefixSetS = fromList [[]], suffixSetE = fromList [[A],[B]], mappingT = fromList [(([],[A]),X),(([],[B]),Y),(([A],[A]),X),(([A],[B]),X),(([B],[A]),X),(([B],[B]),X)]}
-- [A]
-- ObservationTable {
--  prefixSetS = fromList [[],[A]],
--  suffixSetE = fromList [[A],[B]],
--  mappingT = fromList [
--      (([],[A]),X),
--      (([],[B]),Y),
--      (([A],[A]),X),
--      (([A],[B]),X),
--      (([A,A],[A]),X),
--      (([A,A],[B]),Y),
--      (([A,B],[A]),X),
--      (([A,B],[B]),Y),
--      (([B],[A]),X),
--      (([B],[B]),X)]
-- }
-- []
-- ([],[])
-- fromList [
--  ([],[[],[A,A],[A,B]]),
--  ([A],[[A],[B]])
-- ]
