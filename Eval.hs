{-# LANGUAGE LambdaCase #-}

module Eval where

import Data.List (intercalate)

import Ident
import Interval
import CoreCTT

import Debug.Trace

debug :: Bool
debug = False --True

myTrace :: String -> a -> a
myTrace s x = if debug then trace s x else x

lookupType :: Ident -> Ctx -> Value
lookupType s [] = error $ "[lookupType] got unknown identifier " ++ show s
lookupType s ((s',entry):ctx) = if s == s' then
        case entry of
            Decl ty     -> eval ctx ty
            Def  ty def -> eval ctx ty
            Val _       -> lookupType s ctx
    else
        lookupType s ctx

eval :: Ctx -> Term -> Value
eval ctx t = myTrace ("[eval] " ++ show t) $ case t of
    Var s -> case lookup s ctx of
        Nothing -> error $ "[eval] not found var " ++ show s ++ " in ctx"
        Just (Val v)     -> v
        Just (Decl ty)   -> Neutral (Var s) (eval ctx ty)
        Just (Def ty e)  -> eval ctx e
    Universe   -> Universe
    Abst s t e -> Closure s t e ctx
    App e1 e2  -> doApply (eval ctx e1) (eval ctx e2)
    Nat        -> Nat
    Zero       -> Zero
    Succ t     -> Succ (eval ctx t)
    Ind ty base step n -> doInd ty' base' step' n'
        where ty'   = eval ctx ty
              base' = eval ctx base
              step' = eval ctx step
              n'    = eval ctx n
    I  -> I
    Sys sys       -> Sys $ evalSystem ctx sys
    Partial phi t -> foldPartial (evalDisjFormula ctx phi) (eval ctx t)
    Restr sys t   -> foldRestr (evalSystem ctx sys) (eval ctx t)
    otherwise     -> error $ "[eval] got " ++ show t

evalConjFormula :: Ctx -> ConjFormula -> ConjFormula
evalConjFormula ctx conj = foldr substConj conj bindings
    where
        bindings' = filter (\(s,v) -> s `elem` (vars conj)) (getBindings ctx)
        bindings  = map (\case (s,Neutral (Var s') I) -> (s,s')) bindings'

evalDisjFormula :: Ctx -> DisjFormula -> DisjFormula
evalDisjFormula ctx (Disj df) = Disj $ map (evalConjFormula ctx) df

foldRestr :: System -> Value -> Value
foldRestr sys v = (uncurry Restr) $ foldRestr' sys v
    where
        foldRestr' :: System -> Value -> (System,Value)
        foldRestr' sys v = case v of
            Restr sys' v' -> foldRestr' (sys ++ sys') v'
            otherwise     -> (sys,v)

foldPartial :: DisjFormula -> Value -> Value
foldPartial disj v = (uncurry Partial) $ foldPartial' disj v
    where
        foldPartial' :: DisjFormula -> Value -> (DisjFormula,Value)
        foldPartial' disj v = case v of
            Partial disj' v' -> foldPartial' (dnf disj' disj) v'
            otherwise        -> (disj,v)
        dnf :: DisjFormula -> DisjFormula -> DisjFormula
        dnf (Disj df1) (Disj df2) = Disj $
            [cf1 `meet` cf2 | cf1 <- df1, cf2 <- df2]  

evalSystem :: Ctx -> System -> System
evalSystem ctx sys = map (\(phi,t) -> (evalConjFormula ctx phi, eval ctx t)) sys

isThatVar :: Value -> Ident -> Bool
isThatVar (Var s') s = s == s'
isThatVar _ _        = False

-- Evaluates a closure
doApply :: Value -> Value -> Value
doApply fun@(Closure s t e ctx) arg = myTrace ("[doApply] fun, arg = " ++ show arg) $ 
    let ctx' = if s == Ident "" || isThatVar arg s then ctx
               else extend ctx s (Val arg)
    in eval ctx' e
doApply (Restr sys fun) arg = doApply fun arg
doApply (Neutral f fty) arg = myTrace ("[doApply] neutral = " ++ show f ++ ", arg = " ++ show arg) $Neutral (App f arg) (doApply fty arg)

-- Evaluates nat-induction
doInd :: Value -> Value -> Value -> Value -> Value
doInd ty base step n = myTrace ("[doInd]") $ case n of
    Zero     -> myTrace ("[doInd] base case") $ base
    Succ n'  -> myTrace ("[doInd] succ case") $ doApply fun prev
        where
            fun = doApply step n
            prev = doInd ty base step n'
    Neutral n Nat -> myTrace ("[doInd] neutral case: n = " ++ show n ++ ", ty = " ++ show ty) $
        Neutral (Ind ty base step n) (doApply ty (Neutral n Nat)) --neutral value
    otherwise -> error $ "[doInd] got " ++ show n

getNeutralType :: Value -> Value
getNeutralType (Neutral _ ty) = ty
getNeutralType v = error $ "[getNeutralType] got non-neutral term " ++ show v

readBack :: [Ident] -> Value -> Term
readBack used v = case v of
    App fun arg -> App (readBack used fun) (readBack used arg)
    Succ v -> Succ (readBack used v)
    Sys sys -> Sys (mapElems (readBack used) sys)
    Partial phi ty -> Partial phi (readBack used ty)
    Restr sys ty -> Restr (mapElems (readBack used) sys) (readBack used ty)
    Ind ty b e n -> Ind (readBack used ty) (readBack used b) (readBack used e) (readBack used n)
    fun@(Closure s t e ctx) -> let
        used' = used ++ keys ctx
        s'    = newVar used' s
        t'    = readBack used (eval ctx t)
        fun'  = Closure s t e (extend ctx s' (Decl t))
        eVal  = doApply fun' (Neutral (Var s') (eval ctx t))
        e'    = readBack (s' : used') eVal
        in Abst s' t' e'
    Neutral v _ -> readBack used v
    otherwise -> v


normalize :: Ctx -> Term -> Term
normalize ctx e = readBack (keys ctx) (eval ctx e)

{- Linear head reduction -}

headRedV :: Ctx -> Term -> Value
headRedV ctx (Var s) = getLeastEval ctx s
headRedV ctx (App k n) = doApply (headRedV ctx k) (eval ctx n)
headRedV ctx (Ind ty b s k) = doInd (eval ctx ty) (eval ctx b) (eval ctx s) (headRedV ctx k)
headRedV ctx t = eval ctx t

--Gets the least evaluated form of 'x' from context
getLeastEval :: Ctx -> Ident -> Value
getLeastEval ctx s = case lookup s ctx of
    Just (Def _ e) -> eval ctx e
    Just (Decl ty) -> Neutral (Var s) (eval ctx ty)

--Do head reduction step
headRed :: Ctx -> Term -> Term
headRed ctx t = case t of
    Abst s t e -> Abst s t e'
        where e' = headRed (extend ctx s (Decl t)) e
    Succ t     -> Succ (headRed ctx t) 
    otherwise  -> readBack (keys ctx) (headRedV ctx t) 


{- Printing utilities (should be in AbsCTT but these need 'readBack') -}

instance Show Term where
    show t = printTerm' 0 (if debug then t else (readBack (vars t) t))-- (readBack (vars t) t)

printTerm' :: Int -> Term -> String
printTerm' i t = case t of
    Var s    -> show s -- ++ case mty of Nothing -> ":?" ; Just ty -> ":" ++ show ty
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
    App fun arg -> par1 ++ printTerm' (i+1) inner ++ " " ++ intercalate " " printedArgs ++ par2 -- ++ case mty of Nothing -> " :?" ; Just ty -> " :" ++ show ty
        where (inner,args) = collectApps (App fun arg) []
              printedArgs  = map (printTerm' (i+1)) args
    Nat          -> "N"
    Zero         -> "0"
    Succ t       -> par1 ++ "S " ++ show t ++ par2 --if isNum then show (n + 1) else "S" ++ printTerm' (i+1) t
        where (isNum,n) = isNumeral t

    Ind ty b s n -> par1 ++ "ind-N " ++ (printTerm' (i+1) ty) ++ " " ++ (printTerm' (i+1) b) ++ " "
         ++ (printTerm' (i+1) s) ++ " " ++ (printTerm' (i+1) n) ++ par2
    I            -> "I"
    Sys sys      -> showSystem sys
    Partial phi t-> "[" ++ show phi ++ "]" ++ printTerm' 0 t
    Restr sys t  -> showSystem sys ++ printTerm' 0 t
    Closure s tyV e ctx    -> "{" ++ show s ++ " : " ++ show tyV ++ "," ++ show e ++ {-"," ++ showCtx ctx ++-} "}"
    Neutral v t  -> "{{" ++ printTerm' 0 v ++ "}}:" ++ printTerm' (i+1) t 
    where (par1,par2) = if i == 0 then ("","") else ("(",")")


showCtx :: Ctx -> String
showCtx ctx = "[" ++ intercalate ", " (map showEntry (reverse ctx)) ++ "]"

showOnlyShort :: String -> String
showOnlyShort s = if length s < 0 then "..." else s --debug

showEntry :: (Ident,CtxEntry) -> String
showEntry (s,Decl ty) = show s ++ " : " ++ showOnlyShort (show ty)
showEntry (s,Def ty val) = show s ++ " : " ++ showOnlyShort (show ty) ++ " = " ++ showOnlyShort (show val)
showEntry (s,Val val) = show s ++ " => " ++ show val

showSystem :: System -> String
showSystem sys = "[" ++ intercalate ", " (map (\(ff,t) -> "(" ++ show ff ++ ") -> " ++ show t) sys) ++ "]"
