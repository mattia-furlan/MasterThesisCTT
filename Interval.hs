{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Interval where

import Data.List (intercalate,nub,delete)
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
        FTrue -> "True" --1F"
        FFalse -> "False" -- 0F"
        Eq0 s -> show s ++ " = 0"
        Eq1 s -> show s ++ " = 1"
        Diag s1 s2 -> "(" ++ show s1 ++ " = " ++ show s2 ++ ")"
        ff1 :/\: ff2 -> "(" ++ show ff1 ++ " /\\ " ++ show ff2 ++ ")"
        ff1 :\/: ff2 -> "(" ++ show ff1 ++ " \\/ " ++ show ff2 ++ ")"

type DNFFormula = (Set (Set (Ident,Interval)))

trueDNF :: DNFFormula
trueDNF = Set.singleton (Set.empty)

showDNF :: DNFFormula -> String
showDNF dnf = intercalate " \\/ " (map showConj (toList dnf))
    where showConj :: Set (Ident,Interval) -> String
          showConj conj = "(" ++ intercalate " /\\ " (map (\(s,i) -> show s ++ " = " ++ show i) (toList conj)) ++ ")"


getFormulaNames :: Formula -> [Ident]
getFormulaNames = nub . getFormulaNames' --remove duplicates
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
foldOrDNF conjs = if Set.empty `elem` conjs then
        trueDNF
    else
        Set.filter (not . Set.null) (fromList conjs)

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
    else foldOrDNF $ concatMap (toConj . toDirEnv . toList) (toList dnf')
    where
        dnf' = Set.filter (not . Set.null) dnf
        toConj :: DirEnv -> [Set (Ident,Interval)]
        toConj dirs@(ones,zeros,_) = if any (`elem` ones) zeros || any (`elem` zeros) ones then
                []
            else
                [fromList (toSubsts dirs)]

simplify :: Formula -> Formula
simplify = fromDNF . simplifyDNF . toDNF

toDNFList :: Formula -> [[(Ident,Interval)]]
toDNFList ff = Set.toList (Set.map Set.toList dnf)
    where dnf = simplifyDNF (toDNF ff)

singleSubst :: Formula -> (Ident,Interval) -> Formula
singleSubst ff (s,i) = case ff of
    FTrue -> FTrue
    FFalse -> FFalse
    Eq0 s' -> if s == s' then 
            case i of 
                IZero  -> FTrue
                IOne   -> FFalse
                IVar x -> Eq0 x
        else
            Eq0 s'
    Eq1 s' -> if s == s' then 
            case i of 
                IZero  -> FFalse
                IOne   -> FTrue
                IVar x -> Eq1 x
        else
            Eq1 s'
    Diag s1 s2 -> if s == s1 then 
            case i of 
                IZero  -> Eq0 s2
                IOne   -> Eq1 s2
                IVar x -> Diag (if s == s2 then x else s2) x
        else if s == s2 then 
            case i of
                IZero  -> Eq0 s1
                IOne   -> Eq1 s1
                IVar x -> Diag (if s == s1 then x else s1) x
        else
            Diag s1 s2
    ff1 :/\: ff2 -> (singleSubst ff1 (s,i)) `makeAnd` (singleSubst ff2 (s,i))
    ff1 :\/: ff2 -> (singleSubst ff1 (s,i)) `makeOr`  (singleSubst ff2 (s,i))


multipleSubst :: Formula -> [(Ident,Interval)] -> Formula
multipleSubst ff list = foldl singleSubst ff list

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

equalFormulas :: Formula -> Formula -> Bool
equalFormulas ff1 ff2 = (simplifyDNF . toDNF) ff1 == (simplifyDNF . toDNF) ff2

checkCongruence :: Formula -> Formula -> Formula -> Bool
checkCongruence ff ff1 ff2 = checkCongruenceDNF (toDNF ff) ff1 ff2

checkCongruenceDNF :: DNFFormula -> Formula -> Formula -> Bool
checkCongruenceDNF dnf ff1 ff2 = all (\conj -> checkCongruenceConj (toList conj) ff1 ff2) (toList dnf)
    where
        checkCongruenceConj [] ff' ff'' = equalFormulas ff' ff''
        checkCongruenceConj (atom:conj) ff' ff'' = checkCongruenceConj conj (singleSubst ff' (s,i)) (singleSubst ff'' (s,i))
            where (s,i) = atom


{- Directions enviroment -}

type DirEnv = ([Ident],[Ident],[[Ident]]) --zeros, ones, diags

emptyDirEnv :: DirEnv  
emptyDirEnv = ([],[],[])

lookupDir :: Ident -> DirEnv -> Maybe Interval
lookupDir s (zeros,ones,diags) = if s `elem` zeros then
        Just IZero
    else if s `elem` ones then
        Just IOne
    else case filter (s `elem`) diags of
        []     -> Nothing
        part:_ -> Just (IVar (head part))

subst :: DirEnv -> Formula -> Formula
subst (zeros,ones,diags) ff = foldl singleSubst ff substs
    where substs  = substs0 ++ substs1 ++ substsd
          substs0 = map (\s -> (s,IZero)) zeros
          substs1 = map (\s -> (s,IOne)) ones
          substsd = concatMap (\part -> map (\s -> (s,IVar (head part))) part) diags

findPartition :: [[Ident]] -> Ident -> [Ident]
findPartition diags s = case filter (s `elem`) diags of
        [] -> [s]
        l  -> head l

addZero :: DirEnv -> Ident -> DirEnv
addZero (zeros,ones,diags) s = let
    toadd = case filter (s `elem`) diags of
        [] -> [s]
        l  -> head l
    in (toadd ++ zeros,ones,delete toadd diags)

addOne :: DirEnv -> Ident -> DirEnv
addOne (zeros,ones,diags) s = let
    toadd = case filter (s `elem`) diags of
        [] -> [s]
        l  -> head l
    in (zeros,toadd ++ ones,delete toadd diags)

addDiag :: DirEnv -> Ident -> Ident -> DirEnv
addDiag dirs@(zeros,ones,diags) s1 s2 =
    if s1 == s2 then
        dirs
    else if s1 `elem` zeros then
        addZero dirs s2
    else if s2 `elem` zeros then
        addZero dirs s1
    else if s1 `elem` ones then
        addOne dirs s2
    else if s2 `elem` ones then
        addZero dirs s1
    else let diags' = [if s1 `elem` set then s2 : set else if s2 `elem` set then s1 : set
                        else set | set <- diags]
             diags'' = diags' ++  --adding a new partition if s1,s2 are new names (i.e. not found in the partitions list)
                if not (s1 `elem` (concat diags') || s2 `elem` (concat diags')) then [[s1,s2]] else []
             par1 = findPartition diags'' s1
             par2 = findPartition diags'' s2
             --eventually join the two partitions (ex. [i,k] [j,k,l] gets joined into [i,j,k,l])
             diags''' = if par1 /= par2 then (delete par2 (delete par1 diags'')) ++ [par1++par2] else diags''
        in (zeros,ones,diags''')

addConj :: [(Ident,Interval)] -> DirEnv -> DirEnv
addConj conj dirs = foldl addAtomic dirs conj
    where
        addAtomic :: DirEnv -> (Ident,Interval) -> DirEnv
        addAtomic dirs (s,i) = case i of
            IZero   -> addZero dirs s 
            IOne    -> addOne dirs s 
            IVar s' -> addDiag dirs s s'

toSubsts :: DirEnv -> [(Ident,Interval)]
toSubsts (zeros,ones,diags) = substs0 ++ substs1 ++ substsd
    where substs0 = map (\s -> (s,IZero)) zeros
          substs1 = map (\s -> (s,IOne)) ones
          substsd = concatMap (\part -> map (\s -> (s,IVar (head part))) part) diags

toDirEnv :: [(Ident,Interval)] -> DirEnv
toDirEnv conj = addConj conj emptyDirEnv

fromDirEnv :: DirEnv -> [(Ident,Interval)]
fromDirEnv = toSubsts

(+++) :: DirEnv -> DirEnv -> DirEnv
dirs1 +++ dirs2 = toDirEnv $ fromDirEnv dirs1 ++ fromDirEnv dirs2

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

