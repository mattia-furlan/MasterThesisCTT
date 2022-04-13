module Eval where

import Data.List (intercalate)

import Ident
import Interval
import CoreCTT

import Debug.Trace


myTrace s x = x -- trace s x

eval :: Ctx -> DirEnv -> Env -> Term -> Value
eval ctx dirs env t =  case t of
    Var s mty -> case lookupDir s dirs of
        Nothing -> case lookup s env of
            Nothing          -> case mty of
                Nothing -> Var s (Just $ eval ctx dirs env $ lookupType ctx s)
                Just ty -> Var s (Just ty)
            Just (Val v)     -> v
            Just (EDef ty e) -> eval ctx dirs env e
        Just i -> toValue i
    Universe -> Universe
    Abst s t e -> Closure s (eval ctx dirs env t) e (ctx,dirs,env)
    App e1 e2 -> doApply (eval ctx dirs env e1) (eval ctx dirs env e2)
    Nat -> Nat
    Zero -> Zero
    Succ t -> Succ (eval ctx dirs env t)
    Ind ty base step n -> doInd ty' base' step' n'
        where ty'   = eval ctx dirs env ty
              base' = eval ctx dirs env base
              step' = eval ctx dirs env step
              n'    = eval ctx dirs env n
    I  -> I
    I0 -> I0
    I1 -> I1
    Sys sys -> doSys (evalSystem ctx dirs env sys)
    Partial phi t -> doPartial (evalFormula dirs phi) (eval ctx dirs env t)
    Restr phi u t -> Restr (evalFormula dirs phi) (eval ctx dirs env u) (eval ctx dirs env t)

toValue :: Interval -> Value
toValue IZero = I0
toValue IOne  = I1
toValue (IVar s) = Var s (Just I)

toInterval :: Value -> Interval
toInterval I0 = IZero
toInterval I1 = IOne
toInterval (Var s _) = IVar s

evalFormula :: DirEnv -> Formula -> Formula
evalFormula dirs ff = simplify $ subst dirs ff

evalSystem :: Ctx -> DirEnv -> Env -> System -> System
evalSystem ctx dirs env sys = myTrace ("[evalSystem] sys = " ++ show sys ++ ", dirs = " ++ show dirs) $
    map (\(phi,t) -> (evalFormula dirs phi, eval ctx dirs env t)) sys

--Applies substitutions to already formed values (needed when calling `conv`)
simplifyValue :: DirEnv -> Value -> Value
simplifyValue dirs (Var s mty) = case lookupDir s dirs of
    Nothing -> Var s mty
    Just i -> toValue i
simplifyValue dirs (Sys sys) = doSys $ map (\(phi,v) -> (subst dirs phi,v)) sys
simplifyValue dirs (Restr phi u ty) = Restr (subst dirs phi) u ty
simplifyValue dirs (App neutral arg) = neutral @@ (simplifyValue dirs arg)
simplifyValue dirs v = v

-- Evaluates a closure
doApply :: Value -> Value -> Value
doApply (Closure s tVal e (ctx,dirs,env)) arg = case tVal of
    I -> eval (extend ctx s (Decl I)) (addConj [(s,toInterval arg)] dirs) env e
    _ -> eval ctx dirs (extend env s (Val arg)) e  -- I don't need to add `t` to `ctx` (?)
doApply neutral arg = neutral @@ arg -- e.g. reduce `p0` to `a` if `p : Path A a b`


(@@) :: Value -> Value -> Value
neutral @@ arg = myTrace ("[@@] " ++ show neutral ++ " @@ " ++ show arg) $ case neutral of
    Var s (Just (Closure s' I ty (ctx,dirs,rho))) -> let x = doApply (Closure s' I ty (ctx,dirs,rho)) arg in case x of
        Restr FTrue u _ -> myTrace ("[@@] Restr FTrue u _, x = " ++ show x ++ "  ==> evals to " ++ show u) $ u
        otherwise       -> myTrace ("[@@] otherwise, x = " ++ show x) $ App neutral arg
    otherwise -> App neutral arg

-- Evaluates nat-induction
doInd :: Value -> Value -> Value -> Value -> Value
doInd ty base step n = case n of
    Zero     -> base
    Succ n'  -> doApply fun prev
        where
            fun = doApply step n
            prev = doInd ty base step n'
    otherwise -> Ind ty base step n --neutral value

doSys :: System -> Value
doSys sys = case lookup FTrue sys of
    Nothing -> Sys sys
    Just v  -> v

doPartial :: Formula -> Value -> Value
doPartial ff v = case ff of
    FTrue -> v
    _     -> Partial ff v

readBack :: [Ident] -> Value -> Term
readBack used (App fun arg) = App (readBack used fun) (readBack used arg)
readBack used (Succ v) = Succ (readBack used v)
readBack used (Sys sys) = Sys (mapElems (readBack used) sys)
readBack used (Partial phi ty) = Partial phi (readBack used ty)
readBack used (Restr phi u ty) = Restr phi (readBack used u) (readBack used ty)
readBack used (Ind ty b e n) = Ind (readBack used ty) (readBack used b) (readBack used e) (readBack used n)
readBack used fun@(Closure s tVal e (ctx,dirs,env)) = let
        used' = used ++ keys ctx
        s'   = newVar used' s
        t'   = readBack (s' : used') tVal
        eVal = doApply fun (Var s' (Just tVal))
        e'   = readBack (s' : used') eVal
        in Abst s' t' e'
readBack used v = v


normalize :: Ctx -> Term -> Term
normalize ctx e = readBack (keys ctx) (eval ctx emptyDirEnv (ctxToEnv ctx) e)


{- Linear head reduction -}

headRedV :: Ctx -> Term -> Value
headRedV ctx (Var s _) = getLeastEval ctx s
headRedV ctx (App k n) = doApply (headRedV ctx k) (eval ctx emptyDirEnv emptyEnv n)
headRedV ctx (Ind ty b s k) = doInd (eval ctx emptyDirEnv emptyEnv ty) (eval ctx emptyDirEnv emptyEnv b) (eval ctx emptyDirEnv emptyEnv s) (headRedV ctx k)
headRedV ctx t = eval ctx emptyDirEnv emptyEnv t

--Gets the least evaluated form of 'x' from context
getLeastEval :: Ctx -> Ident -> Value
getLeastEval ctx s = case lookup s ctx of
    Just (Def _ e) -> eval ctx emptyDirEnv emptyEnv e
    otherwise      -> Var s Nothing

--Do head reduction step
headRed :: Ctx -> Term -> Term
headRed ctx t = case t of
    Abst s t e -> Abst s t e'
        where e' = headRed (extend ctx s (Decl t)) e
    Succ t     -> Succ (headRed ctx t) 
    otherwise  -> readBack (keys ctx) (headRedV ctx t) 


{- Printing utilities (should be in AbsCTT but these need 'readBack') -}

instance Show Term where
    show t = printTerm' 0 (readBack [] t)

printTerm' :: Int -> Term -> String
printTerm' i t = case t of
    Var s mty    -> show s -- ++ case mty of Nothing -> ":?" ; Just ty -> ":" ++ show ty
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
    I0           -> "I0"
    I1           -> "I1"
    Sys sys      -> showSystem sys
    Partial phi t  -> "[" ++ show phi ++ "]" ++ printTerm' 0 t
    Restr phi u t  -> "[" ++ show phi ++ " -> " ++ printTerm' 0 u ++ "]" ++ printTerm' 0 t
    Closure s tyV e (ctx,dirs,rho)    -> "{" ++ show s ++ "," ++ show e ++ ")}"
    where (par1,par2) = if i == 0 then ("","") else ("(",")")


showCtx :: Ctx -> String
showCtx ctx = "[" ++ intercalate ", " (map showEntry (reverse ctx)) ++ "]"

showOnlyShort :: String -> String
showOnlyShort s = if length s > 1500 then "..." else s

showEntry :: (Ident,CtxEntry) -> String
showEntry (s,Decl ty) = show s ++ " : " ++ showOnlyShort (show ty)
showEntry (s,Def ty val) = show s ++ " : " ++ showOnlyShort (show ty) ++ " = " ++ showOnlyShort (show val)

showEnv :: Env -> String
showEnv env = "[" ++ intercalate ", " (map showEnvEntry (reverse env)) ++ "]"

showEnvEntry :: (Ident,EnvEntry) -> String
showEnvEntry (s,Val val) = show s ++ " -> " ++ show val
showEnvEntry (s,EDef ty val) = show s ++ " : " ++ showOnlyShort (show ty) ++ " = " ++ showOnlyShort (show val)


showSystem :: System -> String
showSystem sys = "[" ++ intercalate ", " (map (\(ff,t) -> "[" ++ show ff ++ "] " ++ show t) sys) ++ "]"
