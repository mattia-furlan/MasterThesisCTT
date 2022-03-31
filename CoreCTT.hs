{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CoreCTT where

import Prelude
import qualified Prelude as C (Eq, Ord, Show, Read)
import Prelude ((++), show, map)
import Data.List (intercalate)
import qualified Data.String

{- Syntax -}

data Term
    = Var Ident
    | Universe
    | Abst Ident Term Term
    | App Term Term
    | Nat
    | Zero
    | Succ Term
    | Ind Term Term Term Term
    -- Parsed terms of the form 'A -> B' get transformed to '[x:A]B' _after_ parsing
    -- (to avoid name shadowing)
    -- | Fun Term Term
  deriving (C.Eq, C.Ord, C.Read)

newtype Program = Program [Toplevel]

data Toplevel = Definition Ident Term Term   -- Type-check and add to the context
              | Example Term                 -- Infer type and normalize 
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data PrintTermMod = AsTerm | AsType
    deriving (Show, Ord, Eq, Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Read, Data.String.IsString)

instance C.Show Ident where
    show (Ident s) = s

instance C.Show Term where
    show t = showTerm t AsTerm

showAbst :: Term -> PrintTermMod -> String
showAbst (Abst s t e) m = "(" ++ abstS ++ ")"
    where
        abstS = if m == AsType && not (containsVar e s)
            then --A -> B
                showTerm t m ++ " -> " ++ showTerm e m
            else
                "[" ++ show s ++ ":" ++ showTerm t m ++ "]" ++ showTerm e m 

showTerm :: Term -> PrintTermMod -> String
showTerm (Var (Ident var)) _ = var
showTerm (Universe) _ = "U"
showTerm abst@(Abst _ _ _) m = showAbst abst m
showTerm (App e1 e2) m = "(" ++ inner' ++ " " ++ (intercalate " " (map (\t -> showTerm t m) args)) ++ ")"
    where
        (inner,args) = collectApps (App e1 e2) []
        inner' = case inner of
            (Abst _ _ _) -> "(" ++ showTerm inner m ++ ")"
            otherwise    -> showTerm inner m
showTerm Nat _ = "N"
showTerm Zero _ = "0"
showTerm (Succ n) m = "S" ++ case n of
  Zero   -> "0"
  Succ _ -> showTerm n m
  t      -> "(" ++ showTerm t m ++ ")"
showTerm (Ind ty base step n) m = "(ind " ++ showTerm ty AsTerm ++ " "
    ++ showTerm base AsTerm ++ " " ++ showTerm step AsTerm ++ " " ++ showTerm n AsTerm ++ ")"

collectApps :: Term -> [Term] -> (Term,[Term])
collectApps t apps = case t of
    App t1 t2' -> collectApps t1 (t2' : apps)
    otherwise -> (t,apps)

-- Generates a new name starting from 'x' (maybe too inefficient - TODO)
newVar :: [Ident] -> Ident -> Ident
newVar used x = if x `elem` used then newVar used (Ident $ show x ++ "'") else x

termVars :: Term -> [Ident]
termVars t = case t of
    Var s        -> [s]
    Universe     -> []
    Abst s t e   -> s : termVars t ++ termVars e
    App fun arg  -> termVars fun ++ termVars arg
    Nat          -> []
    Zero         -> []
    Succ t       -> termVars t
    Ind ty b s n -> termVars ty ++ termVars b ++ termVars s ++ termVars n
    --Fun t1 t2    -> termVars t1 ++ termVars t2

termFreeVars :: Term -> [Ident]
termFreeVars t = case t of
    Var s        -> [s]
    Universe     -> []
    Abst s t e   -> filter ( /= s) (termFreeVars t ++ termFreeVars e)
    App fun arg  -> termFreeVars fun ++ termFreeVars arg
    Nat          -> []
    Zero         -> []
    Succ t       -> termFreeVars t
    Ind ty b s n -> termFreeVars ty ++ termFreeVars b ++ termFreeVars s ++ termFreeVars n
    --Fun t1 t2    -> termFreeVars t1 ++ termFreeVars t2

containsVar :: Term -> Ident -> Bool
containsVar (Var s') s = (s == s')
containsVar (Universe) s = False
containsVar (Abst s' t e) s = ((s /= s') && containsVar t s) || containsVar e s
containsVar (App fun arg) s = containsVar fun s || containsVar arg s
containsVar Nat s = False
containsVar Zero s = False
containsVar (Succ t) s = containsVar t s
containsVar (Ind ty b step n) s = containsVar ty s || containsVar b s || containsVar step s || containsVar n s

checkTermShadowing :: [Ident] -> Term -> Bool
checkTermShadowing vars t = case t of
    (Var s)       -> True
    Universe      -> True
    (Abst (Ident "") t e) -> checkTermShadowing vars t && checkTermShadowing vars e
    (Abst s t e)  -> not (s `elem` vars) &&
        checkTermShadowing (s : vars) t && checkTermShadowing (s : vars) e 
    (App fun arg) -> checkTermShadowing vars fun && checkTermShadowing vars arg
    Nat           -> True
    Zero          -> True
    Succ n        -> checkTermShadowing vars n
    Ind ty b s n  -> checkTermShadowing vars ty && checkTermShadowing vars b &&
        checkTermShadowing vars s && checkTermShadowing vars n


{- Values -}

data Value = VClosure Ident Value Term (Env EnvEntry) -- x : T, E, rho
           | VUniverse
           | VNat
           | VZero
           | VSucc Value
           -- Neutral
           | VVar Ident
           | VApp Value Value
           | VInd Value Value Value Value
    deriving Show

isNeutral :: Value -> Bool
isNeutral v = case v of
    VVar _       -> True
    VApp _ _     -> True
    VInd _ _ _ _ -> True
    otherwise    -> False

{- Printing functions are in 'Eval.hs' -}


{- Contexts -}

newtype Env a = Env [(Ident,a)]
    deriving Show

type ErrorString = String

emptyEnv :: Env a
emptyEnv = Env []

extendEnv :: Env a -> Ident -> a -> Env a
extendEnv (Env env) s v = Env $ (s,v) : env

lookupEnv :: (Show a) => Env a -> Ident -> Either ErrorString a
lookupEnv (Env env) x = case env of
    [] -> Left $ "identifier '" ++ show x ++ "' not found in enviroment " ++ show env
    (s,v) : env' -> if s == x then Right v else lookupEnv (Env env') x

isInEnv :: Env a -> Ident -> Bool
isInEnv env s = s `elem` (getIdentsEnv env)

getIdentsEnv :: Env a -> [Ident]
getIdentsEnv (Env env) = map fst env

getEntriesEnv :: Env a -> [a]
getEntriesEnv (Env env) = map snd env

concatEnv :: Env a -> Env a -> Env a
concatEnv (Env env1) (Env env2) = Env (env1 ++ env2)

--TODO: removeFromEnv should remove also items that depend on the deleted item
removeFromEnv :: Env a -> Ident -> Env a
removeFromEnv (Env []) s = Env []
removeFromEnv (Env ((s',v) : env)) s =
    if s == s' then
        removeFromEnv (Env env) s --Should I keep this? Context should not have shadowed names!
    else
        extendEnv (removeFromEnv (Env env) s) s' v

forceRight :: (Show a) => Either a b -> b
forceRight (Right y) = y
forceRight (Left x) = error $ "something went wrong... " ++ show x


data EnvEntry = Val Value
              | EDef Term Term
    deriving Show

type Ctx = Env CtxEntry

data CtxEntry = Decl Term      -- Type
              | Def Term Term  -- Type and definition
    deriving Show

emptyCtx :: Ctx
emptyCtx = emptyEnv

lookupType :: Ctx -> Ident -> Either ErrorString Term
lookupType ctx s = do
    entry <- lookupEnv ctx s
    case entry of
        Decl ty     -> Right ty
        Def  ty def -> Right ty

ctxToEnv :: Ctx -> Env EnvEntry
ctxToEnv ctx = Env $ concatMap getEnvEntry (zip (getIdentsEnv ctx) (getEntriesEnv ctx))
    where
        getEnvEntry :: (Ident,CtxEntry) -> [(Ident,EnvEntry)]
        getEnvEntry (s,(Decl ty)) = []
        getEnvEntry (s,(Def ty val)) = [(s,(EDef ty val))]

getLockedCtx :: [Ident] -> Ctx -> Ctx
getLockedCtx idents ctx = foldr getLockedCtx' ctx idents
    where
        getLockedCtx' :: Ident -> Ctx -> Ctx
        getLockedCtx' s (Env []) = emptyCtx
        getLockedCtx' s (Env ((s',Def ty def) : ctx)) =
            if s == s' then Env $ (s,Decl ty) : ctx
                       else extendEnv (getLockedCtx' s (Env ctx)) s' (Def ty def)
        getLockedCtx' s (Env ((s',Decl ty) : ctx)) =
            extendEnv (getLockedCtx' s (Env ctx)) s' (Decl ty)


