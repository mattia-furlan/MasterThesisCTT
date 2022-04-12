module Eval where

import Data.List (intercalate)

import Ident
import Interval
import CoreCTT

import Debug.Trace

eval :: Env -> Term -> Value
eval env t = case t of
    Var s _ -> case (lookup s env) of
        Nothing          -> t -- Var s (lookupType ctx s)
        Just (Val v)     -> v
        Just (EDef ty e) -> eval env e
    Universe -> Universe
    Abst s t e -> Closure s (eval env t) e env
    App e1 e2 -> doApply (eval env e1) (eval env e2)
    Nat -> Nat
    Zero -> Zero
    Succ t -> Succ (eval env t)
    Ind ty base step n -> doInd ty' base' step' n'
        where ty'   = eval env ty
              base' = eval env base
              step' = eval env step
              n'    = eval env n
    I  -> I
    I0 -> I0
    I1 -> I1
    Sys sys -> doSys (evalSystem env sys)
    Partial phi t -> doPartial (evalFormula env phi) (eval env t)
    Restr phi u t -> Restr (evalFormula env phi) (eval env u) (eval env t)

evalFormula :: Env -> Formula -> Formula
evalFormula env ff = simplify $ multipleSubst ff env'
    where getIVal :: (Ident,EnvEntry) -> [(Ident,Interval)]
          getIVal (s,v) = case v of
              Val I0         -> [(s,IZero)]
              Val I1         -> [(s,IOne)]
              Val (Var s' _) -> [(s,IVar s')]
              EDef I t       -> [(s,snd $ head $ getIVal (s,Val t))]
              _              -> []
          env' = concatMap getIVal env

evalSystem :: Env -> System -> System
evalSystem env sys = map (\(phi,t) -> (evalFormula env phi, eval env t)) sys

simplifySystem :: DirEnv -> System -> Value
simplifySystem dirs sys = doSys $ map (\(phi,v) -> (subst dirs phi,v)) sys

-- Evaluates a closure
doApply :: Value -> Value -> Value
doApply (Closure s tVal e env) arg = eval (extend env s (Val arg)) e
doApply neutral arg = case neutral of
    Var s (Just (Closure s' I ty rho)) -> case doApply (Closure s' I ty rho) arg of
        Restr FTrue u _ -> u
        otherwise       -> App neutral arg
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
readBack used fun@(Closure s tVal e env) = let
        s'   = newVar used s
        t'   = readBack (s' : used) tVal
        eVal = doApply fun (Var s' Nothing)
        e'   = readBack (s' : used) eVal
        in Abst s' t' e'
readBack used v = v


normalize :: Env -> Term -> Term
normalize env e = readBack (keys env) (eval env e)


{- Linear head reduction -}

headRedV :: Ctx -> Term -> Value
headRedV ctx (Var s _) = getLeastEval ctx s
headRedV ctx (App k n) = doApply (headRedV ctx k) (eval emptyEnv n)
headRedV ctx (Ind ty b s k) = doInd (eval emptyEnv ty) (eval emptyEnv b) (eval emptyEnv s) (headRedV ctx k)
headRedV ctx t = eval emptyEnv t

--Gets the least evaluated form of 'x' from context
getLeastEval :: Ctx -> Ident -> Value
getLeastEval ctx s = case lookup s ctx of
    Just (Def _ e) -> eval emptyEnv e
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
    Var s _      -> show s
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
    --Closure{}    -> "closure"
    where (par1,par2) = if i == 0 then ("","") else ("(",")")


showCtx :: Ctx -> String
showCtx ctx = "[" ++ intercalate ", " (map showEntry (reverse ctx)) ++ "]"

showOnlyShort :: String -> String
showOnlyShort s = if length s > 150 then "..." else s

showEntry :: (Ident,CtxEntry) -> String
showEntry (s,Decl ty) = show s ++ " : " ++ showOnlyShort (show ty)
showEntry (s,Def ty val) = show s ++ " : " ++ showOnlyShort (show ty) ++ " = " ++ showOnlyShort (show val)
showEntry (s,Form ff) = show s ++ " = " ++ showOnlyShort (show ff)

showEnv :: Env -> String
showEnv env = "[" ++ intercalate ", " (map showEnvEntry (reverse env)) ++ "]"

showEnvEntry :: (Ident,EnvEntry) -> String
showEnvEntry (s,Val val) = show s ++ " -> " ++ show val
showEnvEntry (s,EDef ty val) = show s ++ " : " ++ showOnlyShort (show ty) ++ " = " ++ showOnlyShort (show val)
showEnvEntry (s,IVal i) = show s ++ " -> " ++ show i


showSystem :: System -> String
showSystem sys = "[" ++ intercalate ", " (map (\(ff,t) -> "[" ++ show ff ++ "] " ++ show t) sys) ++ "]"
