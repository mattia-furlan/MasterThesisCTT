{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Interval where

import qualified Data.List as List
import Data.Set (Set(..), singleton, empty, toList, fromList)
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import Ident

data Interval = IZero | IOne | IVar Ident
    deriving (Eq, Read)

instance Show Interval where
    show IZero = "0"
    show IOne  = "1"
    show (IVar s) = show s

instance Ord Interval where
    compare IZero (IVar s) = LT
    compare (IVar s) IOne = LT
    compare IZero IOne = LT
    compare IZero IZero = EQ
    compare IOne IOne = EQ
    compare (IVar s1) (IVar s2) = compare s1 s2
    compare (IVar s) IZero = GT
    compare IOne (IVar s) = GT
    compare IOne IZero = GT

data Formula = FTrue
             | FFalse
             | Eq0 Ident
             | Eq1 Ident
             | Diag Ident Ident
             | Formula :/\: Formula
             | Formula :\/: Formula
    deriving (Eq,Ord,Read)

instance Show Formula where
    show ff = case ff of
        FTrue -> "1F"
        FFalse -> "0F"
        Eq0 s -> show s ++ " = 0"
        Eq1 s -> show s ++ " = 1"
        Diag s1 s2 -> "(" ++ show s1 ++ " = " ++ show s2 ++ ")"
        ff1 :/\: ff2 -> "(" ++ show ff1 ++ " /\\ " ++ show ff2 ++ ")"
        ff1 :\/: ff2 -> "(" ++ show ff1 ++ " \\/ " ++ show ff2 ++ ")"

type DNFFormula = (Set (Set (Ident,Interval)))

trueDNF :: DNFFormula
trueDNF = Set.singleton (Set.empty)

showDNF :: DNFFormula -> String
showDNF dnf = List.intercalate " \\/ " (map showConj (toList dnf))
    where showConj :: Set (Ident,Interval) -> String
          showConj conj = "(" ++ List.intercalate " /\\ " (map (\(s,i) -> show s ++ " = " ++ show i) (toList conj)) ++ ")"


getFormulaNames :: Formula -> [Ident]
getFormulaNames = List.nub . getFormulaNames' --remove duplicates
    where getFormulaNames' :: Formula -> [Ident]
          getFormulaNames' ff = case ff of
                FTrue -> []
                FFalse -> []
                Eq0 s -> [s]
                Eq1 s -> [s]
                Diag s1 s2 -> [s1,s2]
                ff1 :/\: ff2 -> getFormulaNames' ff1 ++ getFormulaNames' ff2
                ff1 :\/: ff2 -> getFormulaNames' ff1 ++ getFormulaNames' ff2

makeOr :: Formula -> Formula -> Formula
makeOr FFalse ff2 = ff2
makeOr ff1 FFalse = ff1
makeOr FTrue _ = FTrue
makeOr _ FTrue = FTrue
makeOr ff1 ff2 = ff1 :\/: ff2

makeAnd :: Formula -> Formula -> Formula
makeAnd FFalse _ = FFalse
makeAnd _ FFalse = FFalse
makeAnd FTrue ff2 = ff2
makeAnd ff1 FTrue = ff1
makeAnd ff1 ff2 = ff1 :/\: ff2

foldOr :: [Formula] -> Formula
foldOr [] = FFalse
foldOr ffs = foldl1 makeOr ffs

foldAnd :: [Formula] -> Formula
foldAnd [] = FTrue
foldAnd ffs = foldl1 makeAnd ffs

decomposeOr :: Formula -> [Formula]
decomposeOr (ff1 :\/: ff2) = decomposeOr ff1 ++ decomposeOr ff2
decomposeOr FFalse = []
decomposeOr ff = [ff]

decomposeAnd :: Formula -> [Formula]
decomposeAnd (ff1 :/\: ff2) = decomposeAnd ff1 ++ decomposeAnd ff2
decomposeAnd FTrue = []
decomposeAnd ff = [ff]

getBoundary :: Formula -> Formula
getBoundary ff = foldOr disjunctions
    where names = getFormulaNames ff
          disjunctions = map (\s -> (Eq0 s) :\/: (Eq1 s)) names

foldOrDNF :: [Set (Ident,Interval)] -> DNFFormula
foldOrDNF conjs = if Set.empty `elem` conjs then trueDNF else fromList conjs

toDNF :: Formula -> DNFFormula
toDNF ff = case ff of
    FTrue -> trueDNF
    FFalse -> Set.empty 
    Eq0 s -> Set.singleton $ Set.singleton (s,IZero)
    Eq1 s -> Set.singleton $ Set.singleton (s,IOne)
    Diag s1 s2 -> Set.singleton $ Set.singleton (s1,IVar s2)
    ff1 :/\: ff2 -> foldOrDNF [Set.union conj1 conj2 | conj1 <- dnf1, conj2 <- dnf2]
        where dnf1 = toList $ toDNF ff1
              dnf2 = toList $ toDNF ff2
    ff1 :\/: ff2 -> foldOrDNF (dnf1 ++ dnf2)
        where dnf1 = toList $ toDNF ff1
              dnf2 = toList $ toDNF ff2


fromDNF :: DNFFormula -> Formula
fromDNF (dnf) = foldOr $ map (foldAnd . (map coupleToAtom) . toList) (toList dnf)
    where coupleToAtom (s,IZero) = (Eq0 s)
          coupleToAtom (s,IOne) = (Eq1 s)
          coupleToAtom (s1,IVar s2) = (Diag s1 s2)

simplifyDNF :: DNFFormula -> DNFFormula
simplifyDNF (dnf) = if Set.empty `Set.member` dnf then
        trueDNF
    else Set.map simplifyConj1 dnf'
    where
        dnf' = Set.filter (not . Set.null) dnf

        simplifyConj1 :: Set (Ident,Interval) -> Set (Ident,Interval)
        simplifyConj1 conj = foldl doDelete conj toDelete
            where
                names = map fst (Set.toList conj)
                toDelete = filter (containsBoth conj) names
                containsBoth conj' s = length (filter ( == s) names) > 1
                doDelete conj' s = Set.filter ((/= s) . fst) conj'

simplify :: Formula -> Formula
simplify = fromDNF . simplifyDNF . toDNF

toDNFList :: Formula -> [[(Ident,Interval)]]
toDNFList ff = Set.toList (Set.map Set.toList dnf)
    where dnf = simplifyDNF (toDNF ff)

subst :: Formula -> (Ident,Interval) -> Formula
subst ff (s,i) = case ff of
    FTrue -> FTrue
    FFalse -> FFalse
    Eq0 s' -> if s == s' then 
            case i of 
                IZero  -> FTrue
                IOne   -> FFalse
                IVar x -> Diag s' x
        else
            Eq0 s'
    Eq1 s' -> if s == s' then 
            case i of 
                IZero  -> FFalse
                IOne   -> FTrue
                IVar x -> Diag s' x
        else
            Eq1 s'
    Diag s1 s2 -> if s == s1 then 
            case i of 
                IZero  -> Eq0 s2
                IOne   -> Eq1 s2
                IVar x -> Diag s2 x
        else if s == s2 then 
            case i of 
                IZero  -> Eq0 s1
                IOne   -> Eq1 s1
                IVar x -> Diag s1 x
        else
            Diag s1 s2
    ff1 :/\: ff2 -> (subst ff1 (s,i)) :/\: (subst ff2 (s,i))
    ff1 :\/: ff2 -> (subst ff1 (s,i)) :\/: (subst ff2 (s,i))


multipleSubst :: Formula -> [(Ident,Interval)] -> Formula
multipleSubst ff list = foldl subst ff list

forall :: Ident -> Formula -> Formula
forall s ff = case ff of
    FTrue -> FTrue
    FFalse -> FFalse
    Eq0 s' -> if s == s' then FFalse else ff 
    Eq1 s' -> if s == s' then FFalse else ff
    Diag s1 s2 -> if s == s1 || s == s2 then FFalse else ff
    ff1 :/\: ff2 -> (forall s ff1) `makeAnd` (forall s ff2)
    ff1 :\/: ff2 -> (forall s ff1) `makeOr` (forall s ff2)

{- Less-or-equal: leq = imp -}
imp :: Formula -> Formula -> Bool
imp ff1 ff2 = all (\conj1 -> any (\conj2 -> conj2 `Set.isSubsetOf` conj1) dnf2) dnf1
    where dnf1 = toList . simplifyDNF $ toDNF ff1
          dnf2 = toList . simplifyDNF $ toDNF ff2  --requires simplification!

{- Debug only -}
--dnf :: Formula -> String
--dnf = showDNF . toDNF

equalFormulas :: Formula -> Formula -> Bool
equalFormulas ff1 ff2 = (toDNF ff1) == (toDNF ff2)

checkCongruence :: Formula -> Formula -> Formula -> Bool
checkCongruence ff ff1 ff2 = checkCongruenceDNF (toDNF ff) ff1 ff2

checkCongruenceDNF :: DNFFormula -> Formula -> Formula -> Bool
checkCongruenceDNF dnf ff1 ff2 = all (\conj -> checkCongruenceConj (toList conj) ff1 ff2) (toList dnf)
    where
        checkCongruenceConj [] ff' ff'' = equalFormulas ff' ff''
        checkCongruenceConj (atom:conj) ff' ff'' = checkCongruenceConj conj (subst ff' (s,i)) (subst ff'' (s,i))
            where (s,i) = atom

{- Examples -}

i = Ident "i"
j = Ident "j"
k = Ident "k"

f1 = (Eq0 i) :\/: (Eq1 j)
f2 = ((Eq0 i) :\/: (Eq1 i)) :/\: (Eq1 j)
f3 = ((Eq0 i) :\/: (Eq1 i)) :/\: ((Eq0 j) :\/: ((Eq1 j) :\/: (Eq0 k)))
f4 = ((Eq0 i) :\/: (Eq1 i)) :/\: ((Eq0 j) :\/: ((Eq1 j) :/\: (Eq0 k)))
f5 = ((Eq0 i) :\/: (Eq1 i)) :/\: ((Eq0 j) :\/: ((Eq1 j) :/\: (Eq0 j)))
f6 = ((Eq0 i) :\/: (Eq1 i)) :/\: ((Eq0 j) :\/: ((Eq1 i) :/\: (Eq0 j)))
f7 = FTrue :/\: ((Eq0 i) :/\: (Eq1 j))
f8 = FFalse :\/: FTrue :\/: ((Eq0 i) :/\: (Eq1 j))

