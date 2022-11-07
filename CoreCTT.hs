{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module CoreCTT where

import Data.Maybe (fromJust)

import Ident
import Interval

{- Syntax (terms/values) -}

data Term
    = Var Ident
    | Universe
    {- Let-definition `[x:ty = e]t` -}
    | TDef (Ident,Term,Term) Term
    {- ∏ types -}
    | Abst Ident Term Term
    | App Term Term
    {- ∑ types -}
    | Sigma Ident Term Term
    | Pair Term Term
    | Fst Term
    | Snd Term
    {- Coproducts -}
    | Sum Term Term
    | InL Term
    | InR Term
    --      fam  f_0  f_1   x
    | Split Term Term Term Term
    {- Naturals -}
    | Nat
    | Zero
    | Succ Term
    --    fam  c_0  c_s   n
    | Ind Term Term Term Term
    {- Cubical -}
    | I | I0 | I1
    | Sys System
    | Partial DisjFormula Term
    | Restr System Term
    --     fam      phi      i0   u   base  i
    | Comp Term DisjFormula Term Term Term Term
    {-  For values only: -}
    | Closure Term Ctx   
    --         val  type
    | Neutral Value Value
  deriving (Eq, Ord)

type Value = Term

newtype Program = Program [Toplevel]

data Toplevel = Definition Ident Term Term
              | Declaration Ident Term
              | Example Term
  deriving (Eq, Ord)

-- Useful for printing
{-
isNumeral :: Term -> (Bool,Int)
isNumeral Zero     = (True,0)
isNumeral (Succ t) = (isNum,n + 1)
    where (isNum,n) = isNumeral t
isNumeral _ = (False,0)
-}

-- Generate a fresh name starting from 'x'
newVar :: [Ident] -> Ident -> Ident
newVar used x
    | x `elem` used = newVar used (Ident $ show x ++ "'")
    | otherwise     =  x

-- For printing purposes: e.g. collectApps ((App (App f x_1) x_2) x_3) []
-- returns (f,[x_1,x_2,x_3])
collectApps :: Term -> [Term] -> (Term,[Term])
collectApps t apps = case t of
    App t1 t2' -> collectApps t1 (t2' : apps)
    otherwise  -> (t,apps)

-- Generic class for objects (terms,values,top-levels,formulas,etc.)
-- which contain variables
class SyntacticObject a where
    containsVar :: Ident -> a -> Bool
    containsVar s x = s `elem` freeVars x
    vars :: a -> [Ident]
    freeVars :: a -> [Ident]

instance SyntacticObject Ident where
    vars s = [s]
    freeVars s = [s]

instance SyntacticObject System where
    vars sys = concatMap vars (keys sys) ++ concatMap vars (elems sys)
    freeVars = vars

-- For terms only and not for values (which means we don't
-- define `vars` and `freeVars` for closures and neutral values)
instance SyntacticObject Term where
    vars = \case
        Var s                 -> [s]
        Universe              -> []
        TDef (s,t,e) t'       -> s : vars t ++ vars e ++ vars t'
        Abst s t e            -> s : vars t ++ vars e
        App fun arg           -> vars fun ++ vars arg
        Sigma s t e           -> s : vars t ++ vars e
        Pair t1 t2            -> vars t1 ++ vars t2
        Fst t                 -> vars t
        Snd t                 -> vars t
        Sum ty1 ty2           -> vars ty1 ++ vars ty2
        InL t1                -> vars t1
        InR t2                -> vars t2
        Split ty f1 f2 x      -> vars ty ++ vars f1 ++ vars f2 ++ vars x
        Nat                   -> []
        Zero                  -> []
        Succ t                -> vars t
        Ind ty b s n          -> vars ty ++ vars b ++ vars s ++ vars n
        I                     -> []
        I0                    -> []
        I1                    -> []
        Sys sys               -> vars sys
        Partial phi t         -> vars phi ++ vars t
        Restr sys t           -> vars sys ++ vars t
        Comp fam phi i0 u b i -> vars fam ++ vars phi ++ vars i0 ++ vars u
            ++ vars b ++ vars i
        Closure t ctx         -> vars t ++ keys ctx
        Neutral v _           -> vars v

    freeVars = \case
        Var s                 -> [s]
        Universe              -> []
        TDef (s,t,e) t'       -> freeVars t ++ filter (/= s)
            (freeVars e ++ freeVars t')
        Abst s t e            -> freeVars t ++ filter (/= s) (freeVars e)
        App fun arg           -> freeVars fun ++ freeVars arg
        Sigma s t e           -> freeVars t ++ filter (/= s) (freeVars e)
        Pair t1 t2            -> freeVars t1 ++ freeVars t2
        Fst t                 -> freeVars t
        Snd t                 -> freeVars t
        Sum ty1 ty2           -> freeVars ty1 ++ freeVars ty2
        InL t1                -> freeVars t1
        InR t2                -> freeVars t2
        Split ty f1 f2 x      -> freeVars ty ++ freeVars f1 ++ freeVars f2
            ++ freeVars x
        Nat                   -> []
        Zero                  -> []
        Succ t                -> freeVars t
        Ind ty b s n          -> freeVars ty ++ freeVars b ++ freeVars s
            ++ freeVars n
        I                     -> []
        I0                    -> []
        I1                    -> []
        Sys sys               -> freeVars sys
        Partial phi t         -> freeVars phi ++ freeVars t
        Restr sys t           -> freeVars sys ++ freeVars t
        Comp fam phi i0 u b i -> freeVars fam ++ freeVars phi ++ freeVars i0
            ++ freeVars u ++ freeVars b ++ freeVars i

instance SyntacticObject AtomicFormula where
    vars af = case af of
        Eq0 s        -> [s]
        Eq1 s        -> [s]
        Diag s1 s2   -> [s1,s2]
    freeVars = vars

instance SyntacticObject ConjFormula where
    vars (Conj cf) = concatMap vars cf
    freeVars = vars

instance SyntacticObject DisjFormula where
    vars (Disj df) = concatMap vars df
    freeVars = vars

checkTermShadowing :: [Ident] -> Term -> Bool
checkTermShadowing used term = case term of 
    Var _                 -> True
    Universe              -> True
    TDef (s,t,e) t'       ->
        s `notElem` used && checkTermShadowing used t &&
        checkTermShadowing (if s == Ident "" then used else s : used) e &&
        checkTermShadowing (if s == Ident "" then used else s : used) t'
    Abst s t e           -> s `notElem` used && checkTermShadowing used t &&
        checkTermShadowing (if s == Ident "" then used else s : used) e 
    App fun arg           -> checkTermShadowing used fun &&
        checkTermShadowing used arg
    Sigma s t e           -> s `notElem` used && checkTermShadowing used t &&
        checkTermShadowing (if s == Ident "" then used else s : used) e 
    Pair t1 t2            -> checkTermShadowing used t1 &&
        checkTermShadowing used t2
    Fst t                 -> checkTermShadowing used t
    Snd t                 -> checkTermShadowing used t
    Sum ty1 ty2           -> checkTermShadowing used ty1 &&
        checkTermShadowing used ty2
    InL t1                -> checkTermShadowing used t1
    InR t2                -> checkTermShadowing used t2
    Split ty f1 f2 x      ->
        checkTermShadowing used ty && checkTermShadowing used f1 &&
        checkTermShadowing used f2 && checkTermShadowing used x
    Nat                   -> True
    Zero                  -> True
    Succ n                -> checkTermShadowing used n
    Ind ty b s n          ->
        checkTermShadowing used ty && checkTermShadowing used b &&
        checkTermShadowing used s  && checkTermShadowing used n
    I                     -> True
    I0                    -> True
    I1                    -> True
    Sys sys               -> all (checkTermShadowing used) (elems sys)
    Partial _ t           -> checkTermShadowing used t
    Restr sys t           -> all (checkTermShadowing used) (elems sys) &&
        checkTermShadowing used t
    Comp fam _ i0 u b i   ->
        checkTermShadowing used fam && checkTermShadowing used i0 &&
        checkTermShadowing used u   && checkTermShadowing used b  &&
        checkTermShadowing used i
    otherwise -> error "[checkTermShadowing] got non-term"

{- Printing functions are in 'Eval.hs' -}

type ErrorString = String

{- Generic association lists utilities -}

extend :: Ctx -> Ident -> CtxEntry -> Ctx
extend ctx s e = if s == Ident "" then ctx else (s,e) : ctx

keys :: [(k,a)] -> [k]
keys = map fst

elems :: [(k,a)] -> [a]
elems = map snd

at :: (Eq k) => [(k,a)] -> k -> a
al `at` s = fromJust $ lookup s al

{- Contexts -}

type Ctx = [(Ident,CtxEntry)]

data CtxEntry = Decl Term      -- Type
              | Def Term Term  -- Type and definition
              | Val Value      -- Value binding for `eval`
    deriving (Eq, Ord)

emptyCtx :: Ctx
emptyCtx = []

-- Extract the value bindings from a context
getBindings :: Ctx -> [(Ident,Value)]
getBindings = concatMap $
    \(s,entry) -> case entry of Val v -> [(s,v)]; _ -> []

-- Shall not be called with values in the context
-- (it is used only in `removeFromCtx`)
instance SyntacticObject CtxEntry where
    vars entry = case entry of
        Decl t     -> vars t
        Def ty def -> vars ty ++ vars def
    freeVars entry = case entry of
        Decl t     -> freeVars t
        Def ty def -> freeVars ty ++ freeVars def

-- Remove an identifier from context and also all the others
-- (recursively) which depend on it
removeFromCtx :: Ctx -> Ident -> Ctx
removeFromCtx ctx s = if s `elem` keys ctx then
        let dep  = map fst $ filter (\(_,entry) -> s `elem` freeVars entry) ctx
            ctx' = filter (\(s',_) -> s /= s') ctx
        in foldl removeFromCtx ctx' dep
    else
        ctx

{- Systems -}

type System = [(ConjFormula,Term)]

-- Get the disjunction of the (conjunctive) formulas of the system
getSystemFormula :: System -> DisjFormula
getSystemFormula = Disj . map fst

-- Utility `map` function for systems: it applies a function
-- to the values inside the system
mapSys ::  (Value -> Value) -> System -> System 
mapSys f = map (\(psi,v) -> (psi,f v))

-- Split a type into the form [phi]A, with `phi` eventually trivial
-- It is used in the function `compTypes` of `TypeChecker.hs`
split :: Value -> (DisjFormula,Value)
split v = case v of
    Partial phi ty -> (phi,ty)
    Restr   _   ty -> (fTrue,ty)
    otherwise      -> (fTrue,v)
