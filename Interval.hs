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
        show (Conj cf) = if null cf then
            "True"
        else
            intercalate " /\\ " (map show cf)

instance Show DisjFormula where
        show (Disj df) = if null df then
            "False"
        else
            intercalate " \\/ " (map (\cf -> "(" ++ show cf ++ ")") df)

fTrue :: DisjFormula
fTrue = Disj [Conj []]

fFalse :: DisjFormula
fFalse = Disj []
 
isTrue :: DisjFormula -> Bool
isTrue = (== fTrue)

isFalse :: DisjFormula -> Bool
isFalse = (== fFalse)

{- Implication and equivalence -}

impDisj :: DisjFormula -> DisjFormula -> Bool
impDisj (Disj df1) (Disj df2) = isFalse (Disj df1) || isTrue (Disj df2) ||
    all (\cf1 -> any (impConj cf1) df2) df1

impConj :: ConjFormula -> ConjFormula -> Bool
impConj cf1 cf2 = conjToDirEnv cf1 `impDirEnv` conjToDirEnv cf2

impDirEnv :: DirEnv -> DirEnv -> Bool
impDirEnv dirs1@(zeros1,ones1,diags1) (zeros2,ones2,diags2) = inconsistent dirs1 ||
    zeros2 `isSubset` zeros1 &&
    ones2 `isSubset` ones1 &&
    all (\part2 -> part2 `isSubset` zeros1 || part2 `isSubset` ones1 ||
        any (part2 `isSubset`) diags1) diags2

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

makesTrueAtomic :: DirEnv -> AtomicFormula -> Bool
dirs@(zeros,ones,diags) `makesTrueAtomic` phi = case phi of
    Eq0 s -> s `elem` zeros
    Eq1 s -> s `elem` ones
    Diag s1 s2 -> bothIn zeros || bothIn ones || any bothIn diags
        where bothIn set = s1 `elem` set && s2 `elem` set

makesFalseAtomic :: DirEnv -> AtomicFormula -> Bool
dirs@(zeros,ones,diags) `makesFalseAtomic` phi = case phi of
    Eq0 s -> s `notElem` zeros
    Eq1 s -> s `notElem` ones
    Diag s1 s2 -> not (bothIn zeros && bothIn ones && any bothIn diags)
        && s1 `elem` (getNames dirs) && s2 `elem` (getNames dirs) 
        where bothIn set = s1 `elem` set && s2 `elem` set

makesTrue :: DirEnv -> ConjFormula -> Bool
makesTrue dirs (Conj cf) = all (dirs `makesTrueAtomic`) cf

substAtomic :: (Ident,Ident) -> AtomicFormula -> AtomicFormula
substAtomic (s,s') af = case af of
    Eq0 x | s == x -> Eq0 s'
    Eq1 x | s == x -> Eq1 s'
    Diag x y -> Diag (if x == s then s' else x) (if y == s then s' else y) 
    otherwise -> af

substConj :: (Ident,Ident) -> ConjFormula -> ConjFormula
substConj (s,s') (Conj cf) = Conj $ map (substAtomic (s,s')) cf

substDisj :: (Ident,Ident) -> DisjFormula -> DisjFormula
substDisj (s,s') (Disj df) = Disj $ map (substConj (s,s')) df 

meet :: ConjFormula -> ConjFormula -> ConjFormula
(Conj cf1) `meet` (Conj cf2) = Conj $ cf1 ++ cf2

{-simplifyConjFormula :: DirEnv -> ConjFormula -> ConjFormula
simplifyConjFormula dirs (Conj cf) = Conj $
    if any (dirs `makesFalseAtomic`) cf then
        []
    else
        filter (dirs `makesTrueAtomic`) cf

simplifyDisjFormula :: DirEnv -> DisjFormula -> DisjFormula
simplifyDisjFormula dirs (Disj df) = Disj $
    filter (\(Conj cf) -> cf /= []) $ map (simplifyConjFormula dirs) df-}
{-
i = Ident "i"
j = Ident "j"
k = Ident "k"
l = Ident "l"
-}