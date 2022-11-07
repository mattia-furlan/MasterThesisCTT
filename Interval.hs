{-# LANGUAGE FlexibleInstances #-}

module Interval where

import Data.List (intercalate,delete)
import Ident

-- Atomic formulas are of the kind `i = 0`, `i = 1` or `i = j`
data AtomicFormula
    = Eq0 Ident
    | Eq1 Ident
    | Diag Ident Ident
    deriving (Ord)

-- Equality between atomic formulas
instance Eq AtomicFormula where
    af1 == af2 = case (af1,af2) of
        (Eq0 s1,Eq0 s2) -> s1 == s2
        (Eq1 s1,Eq1 s2) -> s1 == s2
        (Diag s1 s2,Diag s3 s4) -> (s1 == s3 && s2 == s4)
            || (s1 == s4 && s2 == s3)
        otherwise -> False

-- A conjunctive formula is a list of atomic formulas
newtype ConjFormula = Conj [AtomicFormula]
    deriving (Eq,Ord)

-- A disjunctive formula is a list of conjunctive formulas
newtype DisjFormula = Disj [ConjFormula]
    deriving (Eq,Ord)

{- Pretty printing of atomic/conjunctive/disjunctive formulas -}

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
        | otherwise      = intercalate " \\/ " $
            map (\cf -> "(" ++ show cf ++ ")") df

{- Helpers -}

-- True (disjunctive) formula: just one empty conjunction
fTrue :: DisjFormula
fTrue = Disj [Conj []]

-- False formula: empty disjunction
fFalse :: DisjFormula
fFalse = Disj []
 
isTrue :: DisjFormula -> Bool
isTrue = (== fTrue)

isFalse :: DisjFormula -> Bool
isFalse = (== fFalse)

{- Implication and equivalence -}

{- A disjunctive formula implies another one if each of its conjunctions
   makes the second formula true. The case where the second formula is false
   must be handled separately. The first two checks are unnecessary, used only
   for efficiency -}
impDisj :: DirEnv -> DisjFormula -> DisjFormula -> Bool
impDisj dirs (Disj df1) disj2 = isFalse (Disj df1) || isTrue disj2 ||
    if isFalse disj2 then
        all (inconsistent . addConj dirs) df1 -- First formula must be false
    else
        all (\cf1 -> addConj dirs cf1 `makesTrueDisj` disj2) df1

-- Two formulas are equal is they imply each other
eqFormulas :: DirEnv -> DisjFormula -> DisjFormula -> Bool
eqFormulas dirs disj1 disj2 = impDisj dirs disj1 disj2 && impDisj dirs disj2 disj1


{- Directions enviroment -}

-- A `DirEnv` stores the list of zeros, ones and the diagonals partitions
type DirEnv = ([Ident],[Ident],[[Ident]])

emptyDirEnv :: DirEnv  
emptyDirEnv = ([],[],[])

-- A `DirEnv` is inconsistent if there is an identifier
-- which is set both to zero and one
inconsistent :: DirEnv -> Bool
inconsistent (zeros,ones,diags) =
    any (`elem` ones) zeros || any (`elem` zeros) ones

-- Find the partition which contains `s`, if it exists.
-- Otherwise return the fake partition [s]
findPartition :: [[Ident]] -> Ident -> [Ident]
findPartition diags s = case filter (s `elem`) diags of
        [] -> [s]
        l  -> head l

-- Set an identifier to zero. Any eventual identifier which is in the same
-- partition must then be set to zero, and that partition is removed
addZero :: DirEnv -> Ident -> DirEnv
addZero (zeros,ones,diags) s =
    let toadd = findPartition diags s
    in (toadd ++ zeros,ones,delete toadd diags)

-- Set an identifier to one. Any eventual identifier which is in the same
-- partition must then be set to one, and that partition is removed
addOne :: DirEnv -> Ident -> DirEnv
addOne (zeros,ones,diags) s =
    let toadd = findPartition diags s
    in (zeros,toadd ++ ones,delete toadd diags)

-- Add a new diagonal `s1 = s2`
addDiag :: DirEnv -> Ident -> Ident -> DirEnv
addDiag dirs@(zeros,ones,diags) s1 s2
    | s1 == s2        = dirs            -- Trivial, nothing to do
    | s1 `elem` zeros = addZero dirs s2 -- `s1` already zero -> set `s2` to zero
    | s2 `elem` zeros = addZero dirs s1 -- `s2` already zero -> set `s1` to zero
    | s1 `elem` ones  = addOne dirs s2  -- `s1` already one  -> set `s2` to one
    | s2 `elem` ones  = addOne dirs s1  -- `s2` already one  -> set `s1` to zero
    | otherwise = let
        -- Add `s1` and `s2` to the existing partitions if it's the case:
        -- it means that e.g if partition `set` contains `s1`, then `s2`
        -- shall be added to `set` too
        diags' = [if s1 `elem` set then s2 : set else if s2 `elem` set then s1 : set
                else set | set <- diags]
        -- Add a new partition if `s1` and `s2` are new (= not found in the partitions)
        diags'' = diags' ++  
            [[s1,s2] | not (s1 `elem` concat diags' || s2 `elem` concat diags')]
        par1 = findPartition diags'' s1
        par2 = findPartition diags'' s2
        -- Eventually join the two partitions
        -- (e.g. [i,k] [j,k,l] gets joined into [i,j,k,l])
        diags''' = if par1 /= par2 then
                delete par2 (delete par1 diags'') ++ [par1 ++ par2]
            else
                diags''
    in (zeros,ones,diags''')

-- Add a conjunction to a `DirEnv`
addConj :: DirEnv -> ConjFormula -> DirEnv
addConj dirs (Conj conj) = foldl addAtomic dirs conj
    where
        addAtomic :: DirEnv -> AtomicFormula -> DirEnv
        addAtomic dirs' ff = case ff of
            Eq0 s     -> addZero dirs' s 
            Eq1 s     -> addOne dirs' s 
            Diag s s' -> addDiag dirs' s s'

-- Conversion from a conjunction to a `DirEnv`
conjToDirEnv :: ConjFormula -> DirEnv
conjToDirEnv = addConj emptyDirEnv

-- Test if a `DirEnv` makes an atomic formula true.
-- A diagonal is true iff both are zero, or both are true, or if
-- they are in the same partition
makesTrueAtomic :: DirEnv -> AtomicFormula -> Bool
(zeros,ones,diags) `makesTrueAtomic` af = case af of
    Eq0 s -> s `elem` zeros
    Eq1 s -> s `elem` ones
    Diag s1 s2 -> s1 == s2 || bothIn zeros || bothIn ones || any bothIn diags
        where bothIn set = s1 `elem` set && s2 `elem` set

-- A conjunction is true iff all of its atomic formulas are true
makesTrueConj :: DirEnv -> ConjFormula -> Bool
makesTrueConj dirs (Conj cf) = all (dirs `makesTrueAtomic`) cf

-- A disjunction is true iff one of its conjunctive formulas is true
makesTrueDisj :: DirEnv -> DisjFormula -> Bool
makesTrueDisj dirs (Disj df) = any (dirs `makesTrueConj`) df

-- Substitute `s'` for `s` in an atomic formula
substAtomic :: (Ident,Ident) -> AtomicFormula -> AtomicFormula
substAtomic (s,s') af = case af of
    Eq0 x | s == x -> Eq0 s'
    Eq1 x | s == x -> Eq1 s'
    Diag x y -> Diag (if x == s then s' else x) (if y == s then s' else y) 
    otherwise -> af

-- Substitute into each atomic formula of the conjunction
substConj :: (Ident,Ident) -> ConjFormula -> ConjFormula
substConj (s,s') (Conj cf) = Conj $ map (substAtomic (s,s')) cf

-- Concatenation (logical AND) between two conjunctive formulas
meet :: ConjFormula -> ConjFormula -> ConjFormula
(Conj cf1) `meet` (Conj cf2) = Conj $ cf1 ++ cf2