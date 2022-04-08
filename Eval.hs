module Eval where

import qualified Data.List as List
import qualified Data.Map as Map

import Ident
import Interval
import CoreCTT


eval :: Env EnvEntry -> Term -> Value
eval env t = case t of 
    Var s -> case (lookupEnv env s) of
        Left _            -> VVar s
        Right (Val v)     -> v
        Right (EDef ty e) -> eval env e
    Universe -> VUniverse
    Abst s t e -> VClosure s (eval env t) e env
    App e1 e2 -> doApply (eval env e1) (eval env e2)
    Nat -> VNat
    Zero -> VZero
    Succ t -> VSucc (eval env t)
    Ind ty base step n -> doInd ty' base' step' n'
        where ty'   = eval env ty
              base' = eval env base
              step' = eval env step
              n'    = eval env n
    I -> VI
    Sys sys -> doSys (evalSystem env sys)
    Partial phi t -> doPartial (evalFormula env phi) (eval env t)
    --Restr phi u t -> doRestr (evalFormula env phi) (eval env u) (eval env t)

evalFormula :: Env EnvEntry -> Formula -> Formula
evalFormula env ff = multipleSubst ff substs
    where substs = toListEnv (getIVals env)

evalSystem :: Env EnvEntry -> System Term -> System Value
evalSystem env sys = Map.fromList $ map (\(phi,t) -> (evalFormula env phi, eval env t)) (Map.toList sys)


-- Evaluates a closure
doApply :: Value -> Value -> Value
doApply (VClosure s tVal e env) arg = eval (extendEnv env s (Val arg)) e
doApply neutral arg = VApp neutral arg

-- Evaluates nat-induction
doInd :: Value -> Value -> Value -> Value -> Value
doInd ty base step n = case n of
    VZero     -> base
    VSucc n'  -> doApply fun prev
        where
            fun = doApply step n
            prev = doInd ty base step n'    
    otherwise -> VInd ty base step n --neutral value

doSys :: System Value -> Value
doSys sys = case Map.lookup FTrue sys of
    Nothing -> VSys sys
    Just v  -> v

doPartial :: Formula -> Value -> Value
doPartial ff v = case ff of
    FTrue -> v
    _     -> VPartial ff v

readBack :: [Ident] -> Value -> Term
readBack used (VVar s) = Var s
readBack used VUniverse = Universe
readBack used (VApp fun arg) = App (readBack used fun) (readBack used arg)
readBack used VNat = Nat
readBack used VZero = Zero
readBack used (VSucc v) = Succ (readBack used v)
readBack used VI = I
readBack used (VSys sys) = Sys (Map.map (readBack used) sys)
readBack used (VPartial phi v) = Partial phi (readBack used v)
readBack used (VInd ty b e n) = Ind (readBack used ty) (readBack used b) (readBack used e) (readBack used n)
readBack used fun@(VClosure s tVal e env) = let
        s'   = newVar used s
        t'   = readBack (s' : used) tVal
        eVal = doApply fun (VVar s')
        e'   = readBack (s' : used) eVal
        in Abst s' t' e'


normalize :: Env EnvEntry -> Term -> Term
normalize env e = readBack (getIdentsEnv env) (eval env e)


{- Linear head reduction -}

headRedV :: Ctx -> Term -> Value
headRedV ctx (Var s) = getLeastEval ctx s
headRedV ctx (App k n) = doApply (headRedV ctx k) (eval emptyEnv n)
headRedV ctx (Ind ty b s k) = doInd (eval emptyEnv ty) (eval emptyEnv b) (eval emptyEnv s) (headRedV ctx k)
headRedV ctx t = eval emptyEnv t

--Gets the least evaluated form of 'x' from context
getLeastEval :: Ctx -> Ident -> Value
getLeastEval (Env []) s = VVar s
getLeastEval (Env ((s', Decl ty) : ctx)) s =
    if s == s' then VVar s else getLeastEval (Env ctx) s
getLeastEval (Env ((s', Def ty e) : ctx)) s =
    if s == s' then eval emptyEnv e else getLeastEval (Env ctx) s

--Do head reduction step
headRed :: Ctx -> Term -> Term
headRed ctx t = case t of
    Abst s t e -> Abst s t e'
        where e' = headRed (extendEnv ctx s (Decl t)) e
    Succ t     -> Succ (headRed ctx t) 
    otherwise  -> readBack (getIdentsEnv ctx) (headRedV ctx t) 


{- Printing utilities (should be in AbsCTT but these need 'readBack') -}

showValue :: Value -> PrintTermMod -> String
showValue v = showTerm (readBack [] v)

showCtx :: Ctx -> String
showCtx (Env clist) = "[" ++ List.intercalate ", " (map showEntry (reverse clist)) ++ "]"

showOnlyShort :: String -> String
showOnlyShort s = if length s > 150 then "..." else s

showEntry :: (Ident,CtxEntry) -> String
showEntry (s,Decl ty) = show s ++ " : " ++ showOnlyShort (showTerm ty AsType)
showEntry (s,Def ty val) = show s ++ " : " ++ showOnlyShort (showTerm ty AsType) ++ " = " ++ showOnlyShort (showTerm val AsTerm)
showEntry (s,Form ff) = show s ++ " = " ++ showOnlyShort (show ff)

showEnv :: Env EnvEntry -> String
showEnv (Env clist) = "[" ++ List.intercalate ", " (map showEnvEntry (reverse clist)) ++ "]"

showEnvEntry :: (Ident,EnvEntry) -> String
showEnvEntry (s,Val val) = show s ++ " -> " ++ showValue val AsTerm
showEnvEntry (s,EDef ty val) = show s ++ " : " ++ showOnlyShort (showTerm ty AsType) ++ " = " ++ showOnlyShort (showTerm val AsTerm)
showEnvEntry (s,IVal i) = show s ++ " -> " ++ show i

--TODO use `show` instead of `showTerm`