module CoreCTT where

import Prelude
import qualified Prelude as C (Eq, Ord, Show, Read)
import Prelude ((++), show, map)
import Data.List (intercalate)
import Data.Map (Map,toList,fromList,elems,keys)

import Ident
import Interval

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
    {- Cubical -}
    | I
    | Sys (System Term)
    | Partial Formula Term
    | Restr Formula Term Term
  deriving (C.Eq, C.Ord, C.Read)

newtype Program = Program [Toplevel]

data Toplevel = Definition Ident Term Term   -- Type-check and add to the context
              | Example Term                 -- Infer type and normalize 
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data PrintTermMod = AsTerm | AsType
    deriving (Show, Ord, Eq, Read)

instance C.Show Term where
    show t = showTerm t AsTerm


showTerm :: Term -> PrintTermMod -> String
showTerm t m = printTerm t

collectApps :: Term -> [Term] -> (Term,[Term])
collectApps t apps = case t of
    App t1 t2' -> collectApps t1 (t2' : apps)
    otherwise -> (t,apps)

collectAbsts :: Term -> [(Ident,Term)] -> (Term,[(Ident,Term)])
collectAbsts t absts = case t of
    Abst s t e -> collectAbsts e ((s,t) : absts)
    otherwise -> (t,absts)

printTerm :: Term -> String
printTerm = printTerm' 0

printTerm' :: Int -> Term -> String
printTerm' i t = case t of
    Var s        -> show s
    Universe     -> "U"
    Abst s t e   -> par1 ++ abstS ++ par2
        where abstS = if not (containsVar s e)
                then --A -> B
                    printTerm' (i+1) t ++ " -> " ++ printTerm' next e
                else
                    "[" ++ show s ++ ":" ++ printTerm' 0 t ++ "]" ++ printTerm' next e
              next = case e of
                Abst _ _ _ -> 0
                otherwise  -> 0 --i+1
    App fun arg  -> par1 ++ printTerm' (i+1) inner ++ " " ++ intercalate " " printedArgs ++ par2
        where (inner,args) = collectApps (App fun arg) []
              printedArgs  = map (printTerm' (i+1)) args
    Nat          -> "N"
    Zero         -> "0"
    Succ t       -> if isNum then show (n + 1) else "S" ++ printTerm' (i+1) t
        where (isNum,n) = isNumeral t

    Ind ty b s n -> par1 ++ "ind-N " ++ (printTerm' (i+1) ty) ++ " " ++ (printTerm' (i+1) b) ++ " "
         ++ (printTerm' (i+1) s) ++ " " ++ (printTerm' (i+1) n) ++ par2
    I            -> "I"
    Sys sys      -> showSystem sys
    Partial phi t  -> "[" ++ show phi ++ "]" ++ printTerm' 0 t
    Restr phi u t  -> "[" ++ show phi ++ " -> " ++ printTerm' 0 u ++ "]" ++ printTerm' 0 t

    where (par1,par2) = if i == 0 then ("","") else ("(",")")

isNumeral :: Term -> (Bool,Int)
isNumeral Zero     = (True,0)
isNumeral (Succ t) = (isNum,n + 1)
    where (isNum,n) = isNumeral t
isNumeral _ = (False,0)

-- Generates a new name starting from 'x' (maybe too inefficient - TODO)
newVar :: [Ident] -> Ident -> Ident
newVar used x = if x `elem` used then newVar used (Ident $ show x ++ "'") else x

{-
termVars :: Term -> [Ident]
termVars t = case t of
    Var s          -> [s]
    Universe       -> []
    Abst s t e     -> s : termVars t ++ termVars e
    App fun arg    -> termVars fun ++ termVars arg
    Nat            -> []
    Zero           -> []
    Succ t         -> termVars t
    Ind ty b s n   -> termVars ty ++ termVars b ++ termVars s ++ termVars n
    I              -> []
    Restr _ sys t  -> concatMap termVars (elems sys) ++ termVars t
-}
{-
termFreeVars :: Term -> [Ident]
termFreeVars t = case t of
    Var s          -> [s]
    Universe       -> []
    Abst s t e     -> freeVars t ++ filter ( /= s) (freeVars e)
    App fun arg    -> termFreeVars fun ++ termFreeVars arg
    Nat            -> []
    Zero           -> []
    Succ t         -> termFreeVars t
    Ind ty b s n   -> termFreeVars ty ++ termFreeVars b ++ termFreeVars s ++ termFreeVars n
    I              -> []
    Restr _ sys t  -> concatMap termFreeVars (elems sys) ++ termFreeVars t
-}

class SyntacticObject a where
    containsVar :: Ident -> a -> Bool
    containsVar s x = s `elem` (vars x)
    vars :: a -> [Ident]

instance SyntacticObject Term where
    {-containsVar s t = case t of
        Var s' -> s == s'
        Universe -> False
        Abst s' t e -> (s /= s' && containsVar s t) || containsVar s e
        App fun arg -> containsVar s fun || containsVar s arg
        Nat -> False
        Zero -> False
        Succ t -> containsVar s t
        Ind ty b step n -> containsVar s ty || containsVar s b || containsVar s step || containsVar s n
        I -> False
        Sys sys -> any (containsVar s) (keys sys) || any (containsVar s) (elems sys)
        Partial phi t -> containsVar s phi || containsVar s t
        Restr phi u t -> containsVar s phi || containsVar s u || containsVar s t-}

    vars t = case t of
        Var s         -> [s]
        Universe      -> []
        Abst s t e    -> vars t ++ {-filter ( /= s)-} (vars e)
        App fun arg   -> vars fun ++ vars arg
        Nat           -> []
        Zero          -> []
        Succ t        -> vars t
        Ind ty b s n  -> vars ty ++ vars b ++ vars s ++ vars n
        I             -> []
        Sys sys       -> concatMap vars (keys sys) ++ concatMap vars (elems sys)
        Partial phi t -> vars t
        Restr phi u t -> vars u ++ vars t

instance SyntacticObject Formula where
    {-containsVar s ff = case ff of
        FTrue -> False
        FFalse -> False
        Eq0 s' -> s == s'
        Eq1 s' -> s == s'
        Diag s1 s2 -> s == s1 || s == s2 
        ff1 :/\: ff2 -> containsVar s ff1 || containsVar s ff2
        ff1 :\/: ff2 -> containsVar s ff1 || containsVar s ff2-}

    vars ff = case ff of
        FTrue -> []
        FFalse -> []
        Eq0 s' -> [s']
        Eq1 s' -> [s']
        Diag s1 s2 -> [s1,s2]
        ff1 :/\: ff2 -> vars ff1 ++ vars ff2
        ff1 :\/: ff2 -> vars ff1 ++ vars ff2

checkTermShadowing :: [Ident] -> Term -> Bool
checkTermShadowing vars t = case t of
    Var s               -> True
    Universe            -> True
    Abst (Ident "") t e -> checkTermShadowing vars t && checkTermShadowing vars e
    Abst s t e          -> not (s `elem` vars) &&
        checkTermShadowing (s : vars) t && checkTermShadowing (s : vars) e 
    App fun arg         -> checkTermShadowing vars fun && checkTermShadowing vars arg
    Nat                 -> True
    Zero                -> True
    Succ n              -> checkTermShadowing vars n
    Ind ty b s n        -> checkTermShadowing vars ty && checkTermShadowing vars b &&
        checkTermShadowing vars s && checkTermShadowing vars n
    I                   -> True
    Sys sys             -> all (checkTermShadowing vars) (elems sys)
    Partial phi t       -> checkTermShadowing vars t
    Restr phi u t       -> checkTermShadowing vars u && checkTermShadowing vars t

instance Sub Term where
    subst t sub = case t of
        Var s         -> t --TODO change?
        Universe      -> t
        Abst s t e    -> Abst s (subst t sub) (subst e sub)
        App fun arg   -> App (subst fun sub) (subst arg sub)
        Nat           -> t
        Zero          -> t
        Succ t        -> Succ (subst t sub)
        Ind ty b st n -> Ind (subst ty sub) (subst b sub) (subst st sub) (subst n sub)
        I             -> t
        Sys sys       -> Sys $ fromList $ map (\(phi,t) -> (subst phi sub,subst t sub)) (toList sys) --TODO should I simplify now?
        Partial phi t -> Partial (subst phi sub) (subst t sub)  
        Restr phi u t -> Restr (subst phi sub) (subst u sub)  (subst t sub) 

{- Values -}

data Value = VClosure Ident Value Term (Env EnvEntry) -- x : T, E, rho
           | VUniverse
           {- Naturals -}
           | VNat
           | VZero
           | VSucc Value
           {- Cubical -}
           | VI
           | VSys (System Value)
           | VPartial Formula Value
        -- | VRestr Formula Term Term
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

instance Sub Value where
    subst v sub = case v of
        VClosure s' v t rho -> VClosure s' (subst v sub) (subst t sub) rho --TODO (subst rho sub)
        VUniverse           -> VUniverse
        VNat                -> VNat
        VZero               -> VZero
        VSucc n             -> VSucc (subst n sub)
        VI                  -> VI
        VSys sys            -> VSys $ fromList $ map (\(phi,v) -> (subst phi sub,subst v sub)) (toList sys) 
        VPartial phi v      -> VPartial (subst phi sub) (subst v sub)  
        VVar s              -> VVar s
        VApp fun arg        -> VApp (subst fun sub) (subst arg sub)
        VInd ty b st n      -> VInd (subst ty sub) (subst b sub) (subst st sub) (subst n sub)

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

toListEnv :: Env a -> [(Ident,a)]
toListEnv (Env env) = env

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
              | IVal Interval
    deriving Show

type Ctx = Env CtxEntry

data CtxEntry = Decl Term      -- Type
              | Def Term Term  -- Type and definition
              | Form Formula   -- Formula
    deriving Show

emptyCtx :: Ctx
emptyCtx = emptyEnv

lookupType :: Ctx -> Ident -> Either ErrorString Term
lookupType ctx s = do
    entry <- lookupEnv ctx s
    case entry of
        Decl ty     -> Right ty
        Def  ty def -> Right ty
        Form ff     -> Left $ "'" ++ show ff ++ "' is a formula, not a term"

ctxToEnv :: Ctx -> Env EnvEntry
ctxToEnv ctx = Env $ concatMap getEnvEntry (zip (getIdentsEnv ctx) (getEntriesEnv ctx))
    where
        getEnvEntry :: (Ident,CtxEntry) -> [(Ident,EnvEntry)]
        getEnvEntry (s,(Decl ty)) = []
        getEnvEntry (s,(Def ty val)) = [(s,(EDef ty val))]
        getEnvEntry (s,(Form ff)) = [] --TODO

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
        getLockedCtx' s ctx = ctx

getFormulas :: Ctx -> [Formula]
getFormulas (Env ctx) = concatMap (\ce -> case ce of (_,Form phi) -> [phi]; _ -> []) ctx

getIVals :: Env EnvEntry -> Env Interval
getIVals (Env env) = Env $ zip (map fst env) $ concatMap (\e -> case e of IVal i -> [i]; _ -> []) (map snd env) 

{- Cubical -}

-- System Term, System Value
type System a = Map Formula a

showSystem :: (Show a) => System a -> String
showSystem sys = "[" ++ intercalate ", " (map (\(ff,t) -> "[" ++ show ff ++ "] " ++ show t) (toList sys)) ++ "]"


--Orton pitts

