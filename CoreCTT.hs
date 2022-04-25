{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CoreCTT where

import Data.List (intercalate,delete,deleteBy)
import Data.Maybe (fromJust)
import Data.Set (Set(..))
import Control.Monad.State

import Ident
import Interval

{- Syntax (terms/values) -}

data Term
    = Var Ident (Maybe Value)                --Neutral
    | Universe
    | Abst Ident Term Term
    | App Term Term (Maybe Value)            --Neutral
    | Nat
    | Zero
    | Succ Term
    | Ind Term Term Term Term (Maybe Value)  --Neutral
    {- Cubical -}
    | I
    | Sys System
    | Partial DisjFormula Term
    | Restr System Term
    | Comp DisjFormula Term Term Term --TODO
    {- Closure (values only) -}
    | Closure Ident Term Term Ctx 
  deriving (Eq, Ord)

type Value = Term

newtype Program = Program [Toplevel]

data Toplevel = Definition Ident Term Term   -- Type-check and add to the context
              | Declaration Ident Term       -- Check type formation
              | Example Term                 -- Infer type and normalize 
  deriving (Eq, Ord)

isNumeral :: Term -> (Bool,Int)
isNumeral Zero     = (True,0)
isNumeral (Succ t) = (isNum,n + 1)
    where (isNum,n) = isNumeral t
isNumeral _ = (False,0)


-- Generates a new name starting from 'x' (maybe too inefficient - TODO)
newVar :: [Ident] -> Ident -> Ident
newVar used x = if x `elem` used then newVar used (Ident $ show x ++ "'") else x

collectApps :: Term -> [Term] -> (Term,[Term])
collectApps t apps = case t of
    App t1 t2' _ -> collectApps t1 (t2' : apps)
    otherwise    -> (t,apps)

collectAbsts :: Term -> [(Ident,Term)] -> (Term,[(Ident,Term)])
collectAbsts t absts = case t of
    Abst s t e -> collectAbsts e ((s,t) : absts)
    otherwise -> (t,absts)

class SyntacticObject a where
    containsVar :: Ident -> a -> Bool
    containsVar s x = s `elem` (freeVars x)
    vars :: a -> [Ident]
    freeVars :: a -> [Ident]

instance SyntacticObject Ident where
    vars s = [s]
    freeVars s = [s]

instance SyntacticObject System where
    vars sys = concatMap vars (keys sys) ++ concatMap vars (elems sys)
    freeVars = vars

instance SyntacticObject Term where
    vars t = case t of
        Var s _           -> [s]
        Universe          -> []
        Abst s t e        -> vars t ++ vars e
        App fun arg _     -> vars fun ++ vars arg
        Nat               -> []
        Zero              -> []
        Succ t            -> vars t
        Ind ty b s n _    -> vars ty ++ vars b ++ vars s ++ vars n
        I                 -> []
        Sys sys           -> vars sys
        Partial phi t     -> vars phi ++ vars t
        Restr sys t       -> vars sys ++ vars t
        Comp psi x0 fam u -> vars psi ++ vars x0 ++ vars fam ++ vars u
        Closure s t e ctx -> [s] ++ keys ctx
    freeVars t = case t of
        Var s _           -> [s]
        Universe          -> []
        Abst s t e        -> freeVars t ++ filter (/= s) (freeVars e)
        App fun arg _     -> freeVars fun ++ freeVars arg
        Nat               -> []
        Zero              -> []
        Succ t            -> freeVars t
        Ind ty b s n _    -> freeVars ty ++ freeVars b ++ freeVars s ++ freeVars n
        I                 -> []
        Sys sys           -> freeVars sys
        Partial phi t     -> freeVars phi ++ freeVars t
        Restr sys t       -> freeVars sys ++ freeVars t
        Comp psi x0 fam u -> freeVars psi ++ freeVars x0 ++ freeVars fam ++ freeVars u
        Closure s t e ctx -> keys ctx

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
checkTermShadowing vars t = case t of
    Var s _             -> True
    Universe            -> True
    Abst (Ident "") t e -> checkTermShadowing vars t && checkTermShadowing vars e
    Abst s t e          -> s `notElem` vars &&
        checkTermShadowing (s : vars) t && checkTermShadowing (s : vars) e 
    App fun arg _       -> checkTermShadowing vars fun && checkTermShadowing vars arg
    Nat                 -> True
    Zero                -> True
    Succ n              -> checkTermShadowing vars n
    Ind ty b s n _      -> checkTermShadowing vars ty && checkTermShadowing vars b &&
        checkTermShadowing vars s && checkTermShadowing vars n
    I                   -> True
    Sys sys             -> all (checkTermShadowing vars) (elems sys)
    Partial phi t       -> checkTermShadowing vars t
    Restr sys t         -> all (checkTermShadowing vars) (elems sys) && checkTermShadowing vars t
    Comp psi x0 fam u   -> checkTermShadowing vars x0 && checkTermShadowing vars fam && checkTermShadowing vars u


{- Printing functions are in 'Eval.hs' -}

{- Contexts -}

type ErrorString = String

{- Generic association lists utilities -}

--lookup :: (Eq a) => a -> [(a, b)] -> Maybe b --already defined in the Prelude

extend :: [(k,a)] -> k -> a -> [(k,a)]
extend al s v = (s,v) : al

keys :: [(k,a)] -> [k]
keys = map fst

elems :: [(k,a)] -> [a]
elems = map snd

mapElems :: (a -> b) -> [(k,a)] -> [(k,b)]
mapElems f = map (\(s,v) -> (s,f v))

at :: (Eq k) => [(k,a)] -> k -> a
al `at` s = fromJust (lookup s al)

{- Contexts -}

type Ctx = [(Ident,CtxEntry)]

data CtxEntry = Decl Term      -- Type
              | Def Term Term  -- Type and definition
              | Val Value      -- For `eval`
    deriving (Eq, Ord)

emptyCtx :: Ctx
emptyCtx = []

getBindings :: Ctx -> [(Ident,Value)]
getBindings ctx = map (\(s,Val v) -> (s,v))$ 
    filter (\(_,entry) -> case entry of Val _ -> True; _ -> False) ctx

instance SyntacticObject CtxEntry where
    vars entry = case entry of
        Decl t     -> vars t
        Def ty def -> vars ty ++ vars def
        Val _      -> [] --TODO
    freeVars entry = case entry of
        Decl t     -> freeVars t
        Def ty def -> freeVars ty ++ freeVars def
        Val _      -> [] --TODO

getLockedCtx :: [Ident] -> Ctx -> Ctx
getLockedCtx idents ctx = foldr getLockedCtx' ctx idents
    where
        getLockedCtx' :: Ident -> Ctx -> Ctx
        getLockedCtx' s ((s',Def ty def) : ctx) =
            if s == s' then (s,Decl ty) : ctx
                       else (s',Def ty def) : getLockedCtx' s ctx
        getLockedCtx' s ((s',Decl ty) : ctx) =
            (s',Decl ty) : getLockedCtx' s ctx
        getLockedCtx' s ctx = ctx

removeFromCtx :: Ctx -> Ident -> Ctx
removeFromCtx ctx s = if s `elem` (keys ctx) then
        let fall = map fst $ filter (\(_,entry) -> s `elem` (freeVars entry) ) ctx
            ctx' = filter (\(s',_) -> s /= s') ctx
        in foldl removeFromCtx ctx' fall
    else
        ctx


{- Cubical -}

type System = [(ConjFormula,Term)]

getSystemFormula :: System -> DisjFormula
getSystemFormula = Disj . map fst

getFormula :: Value -> DisjFormula
getFormula v = case v of
    Sys sys       -> getSystemFormula sys
    Partial phi v -> phi
    Restr sys v   -> getSystemFormula sys
    otherwise     -> Disj $ []

isPartial :: Value -> Bool
isPartial v = case v of
    Partial _ _ -> True
    otherwise   -> False

isRestr :: Value -> Bool
isRestr v = case v of
    Restr _ _   -> True
    otherwise   -> False

splitPartial :: Value -> (DisjFormula,Value)
splitPartial v = case v of
    Partial phi ty -> (phi,ty)
    otherwise      -> (Disj [Conj []],v)

splitRestr :: Value -> (System,Value)
splitRestr v = case v of
    Restr sys ty -> (sys,ty)
    otherwise    -> ([],v)

getMsg :: Bool -> String -> String
getMsg flag s = if flag then s else ""  

{- State monad for type-checking and evaluation -}

{-
type Env = (Ctx,DirEnv,[String])

type EnvState a = State Env a

ctx :: EnvState Ctx
ctx = do
    (ctx,dirs,logs) <- get
    return ctx

dirs :: EnvState DirEnv
dirs = do
    (ctx,dirs,logs) <- get
    return dirs

addlog :: String -> EnvState ()
addlog s = do
    (ctx,dirs,logs) <- get
    put (ctx,dirs,s : logs)
-}

