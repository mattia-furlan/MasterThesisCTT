module CoreCTT where

import Data.List (intercalate,delete,deleteBy)
--import Data.Map (Map,toList,fromList,elems,keys)
import Data.Maybe (fromJust)

import Ident
import Interval

{- Syntax (terms/values) -}

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
    -- | I0 | I1 --TODO
    | Sys System
    | Partial Formula Term
    | Restr Formula Term Term
    {- Closure (values only) -}
    | Closure Ident Value Term Env 
  deriving (Eq, Ord)

type Value = Term

newtype Program = Program [Toplevel]

data Toplevel = Definition Ident Term Term   -- Type-check and add to the context
              | Example Term                 -- Infer type and normalize 
  deriving (Eq, Ord)

collectApps :: Term -> [Term] -> (Term,[Term])
collectApps t apps = case t of
    App t1 t2' -> collectApps t1 (t2' : apps)
    otherwise -> (t,apps)

collectAbsts :: Term -> [(Ident,Term)] -> (Term,[(Ident,Term)])
collectAbsts t absts = case t of
    Abst s t e -> collectAbsts e ((s,t) : absts)
    otherwise -> (t,absts)

isNumeral :: Term -> (Bool,Int)
isNumeral Zero     = (True,0)
isNumeral (Succ t) = (isNum,n + 1)
    where (isNum,n) = isNumeral t
isNumeral _ = (False,0)

-- Generates a new name starting from 'x' (maybe too inefficient - TODO)
newVar :: [Ident] -> Ident -> Ident
newVar used x = if x `elem` used then newVar used (Ident $ show x ++ "'") else x

class SyntacticObject a where
    containsVar :: Ident -> a -> Bool
    containsVar s x = s `elem` (vars x)
    vars :: a -> [Ident]

instance SyntacticObject Ident where
    vars s = [s]

instance SyntacticObject Term where
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

isNeutral :: Value -> Bool
isNeutral v = case v of
    Var _       -> True
    App _ _     -> True
    Ind _ _ _ _ -> True
    otherwise    -> False

{- Printing functions are in 'Eval.hs' -}

{- Contexts -}

type ErrorString = String

type Env = [(Ident,EnvEntry)]

emptyEnv :: Env
emptyEnv = []

extend :: [(k,a)] -> k -> a -> [(k,a)]
extend al s v = (s,v) : al

keys :: [(k,a)] -> [k]
keys al = map fst al

elems :: [(k,a)] -> [a]
elems al = map snd al

mapElems :: (a -> b) -> [(k,a)] -> [(k,b)]
mapElems f al = map (\(s,v) -> (s,f v)) al

at :: (Eq k) => [(k,a)] -> k -> a
al `at` s = fromJust (lookup s al)

removeFromCtx :: Ctx -> Ident -> Ctx
removeFromCtx ctx s = if s `elem` (keys ctx) then
        let fall = map fst $ filter (\(_,entry) -> s `elem` (vars entry) ) ctx
            ctx' = filter (\(s',_) -> s /= s') ctx
        in foldl removeFromCtx ctx' fall
    else
        ctx

data EnvEntry = Val Value
              | EDef Term Term
              | IVal Interval
    deriving (Eq, Ord)

type Ctx = [(Ident,CtxEntry)]

emptyCtx :: Ctx
emptyCtx = []

data CtxEntry = Decl Term      -- Type
              | Def Term Term  -- Type and definition
              | Form Formula   -- Formula
    deriving (Eq, Ord)

instance SyntacticObject CtxEntry where
    vars entry = case entry of
        Decl t     -> vars t
        Def ty def -> vars ty ++ vars def
        Form ff    -> vars ff

type DirCtx = ([Ident],[Ident],[[Ident]]) --zeros, ones, diags

emptyDirCtx :: DirCtx
emptyDirCtx = ([],[],[])

findPartition :: [[Ident]] -> Ident -> [Ident]
findPartition diags s = case filter (s `elem`) diags of
        [] -> [s]
        l  -> head l

addZero :: DirCtx -> Ident -> DirCtx
addZero (zeros,ones,diags) s = let
    toadd = case filter (s `elem`) diags of
        [] -> [s]
        l  -> head l
    in (toadd ++ zeros,ones,delete toadd diags)

addOne :: DirCtx -> Ident -> DirCtx
addOne (zeros,ones,diags) s = let
    toadd = case filter (s `elem`) diags of
        [] -> [s]
        l  -> head l
    in (zeros,toadd ++ ones,delete toadd diags)

addDiag :: DirCtx -> Ident -> Ident -> DirCtx
addDiag dirctx@(zeros,ones,diags) s1 s2 =
    if s1 `elem` zeros then
        addZero dirctx s2
    else if s2 `elem` zeros then
        addZero dirctx s1
    else if s1 `elem` ones then
        addOne dirctx s2
    else if s2 `elem` ones then
        addZero dirctx s1
    else let diags' = [if s1 `elem` set then s2 : set else if s2 `elem` set then s1 : set
                        else set | set <- diags]
             diags'' = diags' ++  --adding a new partition if s1,s2 are new names (i.e. not found in the partitions list)
                if not (s1 `elem` (concat diags') || s2 `elem` (concat diags')) then [[s1,s2]] else []
             par1 = findPartition diags'' s1
             par2 = findPartition diags'' s2
             --eventually join the two partitions (ex. [i,k] [j,k,l] gets joined into [i,j,k,l])
             diags''' = if par1 /= par2 then (delete par2 (delete par1 diags'')) ++ [par1++par2] else diags''
        in (zeros,ones,diags''')


lookupType :: Ctx -> Ident -> Either ErrorString Term
lookupType ctx s = do
    let mentry = lookup s ctx
    case mentry of
        Nothing -> Left $ "identifier '" ++ show s ++ "' not found in context"
        Just entry -> case entry of
            Decl ty     -> Right ty
            Def  ty def -> Right ty
            Form ff     -> Left $ "'" ++ show ff ++ "' is a formula, not a term"

ctxToEnv :: Ctx -> Env
ctxToEnv ctx = concatMap getEnvEntry (zip (keys ctx) (elems ctx))
    where
        getEnvEntry :: (Ident,CtxEntry) -> [(Ident,EnvEntry)]
        getEnvEntry (s,(Decl ty)) = []
        getEnvEntry (s,(Def ty val)) = [(s,(EDef ty val))]
        getEnvEntry (s,(Form ff)) = [] --TODO

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

getFormulas :: Ctx -> [Formula]
getFormulas ctx = concatMap (\ce -> case ce of (_,Form phi) -> [phi]; _ -> []) ctx

--getIVals :: Env EnvEntry -> Env Interval
--getIVals env = Env $ zip (map fst env) $ concatMap (\e -> case e of IVal i -> [i]; _ -> []) (map snd env) 

{- Cubical -}

type System = [(Formula,Term)]


--Orton pitts

