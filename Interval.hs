{-# LANGUAGE FlexibleInstances #-}

module Interval where

import Data.List (intercalate,nub,delete)
import Data.Set (Set(..), singleton, empty, toList, fromList)
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import Ident

data AtomicFormula
    = Eq0 Ident
    | Eq1 Ident
    | Diag Ident Ident
    deriving (Ord)

instance Eq AtomicFormula where
    af1 == af2 = case (af1,af2) of
        (Eq0 s1,Eq0 s2) -> s1 == s2
        (Eq1 s1,Eq1 s2) -> s1 == s2
        (Diag s1 s2,Diag s3 s4) -> (s1 == s2 && s2 == s4)
            || (s1 == s4 || s2 == s3)
        otherwise -> False

newtype ConjFormula = Conj [AtomicFormula]
    deriving (Eq,Ord)

newtype DisjFormula = Disj [ConjFormula]
    deriving (Eq,Ord)

instance Show AtomicFormula where
    show af = case af of
        Eq0 s -> show s ++ " = 0"
        Eq1 s -> show s ++ " = 1"
        Diag s1 s2 -> show s1 ++ " = " ++ show s2

instance Show ConjFormula where
    show (Conj cf)
        | null cf   = "True"
        | otherwise =  intercalate " /\\ " (map show cf)

instance Show DisjFormula where
    show disj@(Disj df)
        | disj == fFalse = "False"
        | disj == fTrue  = "True"
        | otherwise      = intercalate " \\/ " $ map (\cf -> "(" ++ show cf ++ ")") df

fTrue :: DisjFormula
fTrue = Disj [Conj []]

fFalse :: DisjFormula
fFalse = Disj []
 
isTrue :: DisjFormula -> Bool
isTrue = (== fTrue)

isFalse :: DisjFormula -> Bool
isFalse = (== fFalse)

{- Implication and equivalence -}

impDisj :: DirEnv -> DisjFormula -> DisjFormula -> Bool
impDisj dirs (Disj df1) disj2 = isFalse (Disj df1) || isTrue disj2 ||
    if isFalse disj2 then
        all (inconsistent . addConj dirs) df1
    else
        all (\cf1 -> (addConj dirs cf1) `makesTrueDisj` disj2) df1

eqFormulas :: DirEnv -> DisjFormula -> DisjFormula -> Bool
eqFormulas dirs disj1 disj2 = impDisj dirs disj1 disj2 && impDisj dirs disj2 disj1

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset sub sup = all (`elem` sup) sub

{- Directions enviroment -}

type DirEnv = ([Ident],[Ident],[[Ident]]) --zeros, ones, diags

emptyDirEnv :: DirEnv  
emptyDirEnv = ([],[],[])

getNames :: DirEnv -> [Ident]
getNames (zeros,ones,diags) = zeros ++ ones ++ concat diags

inconsistent :: DirEnv -> Bool
inconsistent (zeros,ones,diags) =
    any (`elem` ones) zeros || any (`elem` zeros) ones

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
addDiag dirs@(zeros,ones,diags) s1 s2
    | s1 == s2        = dirs
    | s1 `elem` zeros = addZero dirs s2
    | s2 `elem` zeros = addZero dirs s1
    | s1 `elem` ones  = addOne dirs s2
    | s2 `elem` ones  = addOne dirs s1
    | otherwise =
    let diags' = [if s1 `elem` set then s2 : set else if s2 `elem` set then s1 : set
                else set | set <- diags]
        diags'' = diags' ++  --adding a new partition if s1,s2 are new names (i.e. not found in the partitions list)
            if not (s1 `elem` (concat diags') || s2 `elem` (concat diags')) then [[s1,s2]] else []
        par1 = findPartition diags'' s1
        par2 = findPartition diags'' s2
         --eventually join the two partitions (ex. [i,k] [j,k,l] gets joined into [i,j,k,l])
        diags''' = if par1 /= par2 then
                delete par2 (delete par1 diags'') ++ [par1 ++ par2]
            else
                diags''
    in (zeros,ones,diags''')

addConj :: DirEnv -> ConjFormula -> DirEnv
addConj dirs (Conj conj) = foldl addAtomic dirs conj
    where
        addAtomic :: DirEnv -> AtomicFormula -> DirEnv
        addAtomic dirs ff = case ff of
            Eq0 s     -> addZero dirs s 
            Eq1 s     -> addOne dirs s 
            Diag s s' -> addDiag dirs s s'

conjToDirEnv :: ConjFormula -> DirEnv
conjToDirEnv = addConj emptyDirEnv

--Assumes `env` is not inconsistent
toConj :: DirEnv -> ConjFormula
toConj env@(zeros,ones,dirs) = Conj $ map (\s -> (Eq0 s)) zeros ++ map (\s -> (Eq1 s)) ones
    ++ concatMap (\part -> map ((\s -> Diag (head part) s)) $ tail part) dirs

makesTrueAtomic :: DirEnv -> AtomicFormula -> Bool
dirs@(zeros,ones,diags) `makesTrueAtomic` phi = case phi of
    Eq0 s -> s `elem` zeros
    Eq1 s -> s `elem` ones
    Diag s1 s2 -> bothIn zeros || bothIn ones || any bothIn diags
        where bothIn set = s1 `elem` set && s2 `elem` set

makesTrueConj :: DirEnv -> ConjFormula -> Bool
makesTrueConj dirs (Conj cf) = all (dirs `makesTrueAtomic`) cf

makesTrueDisj :: DirEnv -> DisjFormula -> Bool
makesTrueDisj dirs (Disj df) = any (dirs `makesTrueConj`) df

substAtomic :: (Ident,Ident) -> AtomicFormula -> AtomicFormula
substAtomic (s,s') af = case af of
    Eq0 x | s == x -> Eq0 s'
    Eq1 x | s == x -> Eq1 s'
    Diag x y -> Diag (if x == s then s' else x) (if y == s then s' else y) 
    otherwise -> af

substConj :: (Ident,Ident) -> ConjFormula -> ConjFormula
substConj (s,s') (Conj cf) = Conj $ map (substAtomic (s,s')) cf

meet :: ConjFormula -> ConjFormula -> ConjFormula
(Conj cf1) `meet` (Conj cf2) = Conj $ cf1 ++ cf2

{-
test :: Bool
test = let 
    i = Ident "i"
    j = Ident "j"
    k = Ident "k"
    l = Ident "l"
    d = Disj
    c = Conj
    ede = emptyDirEnv
    imp = impDisj
    nimp dirs p1 p2 = not (imp dirs p1 p2)
    contra = \p -> imp ede p fFalse
    in
    imp ede fTrue fTrue &&
    nimp ede fTrue fFalse &&
    imp ede fFalse fTrue &&
    imp ede fFalse fFalse &&
    contra (d [c [Eq0 i,Eq1 i]]) &&
    contra (d [c [Eq0 i,Diag i j,Eq1 j]]) &&
    not (contra (d [c [Eq0 i,Diag i j,Diag j i]])) &&
    imp ede (d [c [Eq0 i,Diag i k,Diag j i]]) (d [c [Eq0 i,Eq0 j]]) &&
    imp (conjToDirEnv (Conj $ [Eq0 k])) (d [c [Eq0 i,Diag i k,Diag j i]]) (d [c [Eq0 i,Eq0 j,Diag j k]]) &&
    imp (conjToDirEnv (Conj $ [Eq0 k,Eq1 i])) (d [c [Eq0 i,Diag i k,Diag j i]]) (d [c [Eq0 i,Eq0 j,Diag j k]]) &&
    imp (conjToDirEnv (Conj $ [Eq0 k,Eq1 i])) (d [c [Diag i k,Diag j i]]) fFalse &&
    imp (conjToDirEnv (Conj $ [Eq0 k,Eq1 i])) (d [c [Eq1 j,Diag k k,Diag j i]]) (d [c [Eq1 j,Eq1 i]])
-}


