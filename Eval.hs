{-# LANGUAGE LambdaCase #-}

module Eval where

import Data.List (intercalate)

import Ident
import Interval
import CoreCTT

import Debug.Trace

debug :: Bool
debug = True

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
eval ctx t = {-myTrace ("[eval] " ++ show t ++ ", ctx = " ++ showCtx ctx) $-} case t of
    Var s -> case lookup s ctx of
        Nothing -> error $ "[eval] not found var " ++ show s ++ " in ctx"
        Just (Val v)     -> v
        Just (Decl ty)   -> Neutral (Var s) (eval ctx ty)
        Just (Def ty e)  -> eval ctx e
    Universe    -> Universe
    Abst s ty e -> Closure t ctx
    App e1 e2   -> doApply (eval ctx e1) (eval ctx e2)
    Sigma s ty e-> Closure t ctx
    Pair t1 t2  -> Pair (eval ctx t1) (eval ctx t2)
    Fst t       -> doFst (eval ctx t)
    Snd t       -> doSnd (eval ctx t)
    Nat         -> Nat
    Zero        -> Zero
    Succ t      -> Succ (eval ctx t)
    Ind ty base step n -> doInd ty' base' step' n'
        where ty'   = eval ctx ty
              base' = eval ctx base
              step' = eval ctx step
              n'    = eval ctx n
    I  -> I
    Sys sys       -> Sys $ evalSystem ctx sys
    Partial phi t -> foldPartial (evalDisjFormula ctx phi) (eval ctx t)
    Restr sys t   -> foldRestr (evalSystem ctx sys) (eval ctx t)
    Comp fam u    -> doComp (eval ctx fam) (eval ctx u)
    otherwise     -> error $ "[eval] got " ++ show t

evalConjFormula :: Ctx -> ConjFormula -> ConjFormula
evalConjFormula ctx conj = foldr substConj conj bindings
    where
        bindings' = filter (\(s,v) -> s `elem` (vars conj)) (getBindings ctx)
        bindings  = map (\case (s,Neutral (Var s') I) -> (s,s')) bindings'

evalDisjFormula :: Ctx -> DisjFormula -> DisjFormula
evalDisjFormula ctx (Disj df) = Disj $ map (evalConjFormula ctx) df

foldRestr :: System -> Value -> Value
foldRestr sys v = uncurry Restr $ foldRestr' sys v
    where
        foldRestr' :: System -> Value -> (System,Value)
        foldRestr' sys v = case v of
            Restr sys' v' -> foldRestr' (sys ++ sys') v'
            otherwise     -> (sys,v)

foldPartial :: DisjFormula -> Value -> Value
foldPartial disj v = uncurry Partial $ foldPartial' disj v
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

extract :: Value -> (Ident -> Term -> Term -> Value,Ident,Term,Term)
extract (Abst s t e) = (Abst,s,t,e)
extract (Sigma s t e) = (Sigma,s,t,e)
extract v = error $ "[extract] got " ++ show v

evalClosure :: Value -> Value -> Value
evalClosure (Closure (Abst s t e) ctx) arg  = eval (if s == Ident "" then ctx else extend ctx s (Val arg)) e
evalClosure (Closure (Sigma s t e) ctx) arg = eval (if s == Ident "" then ctx else extend ctx s (Val arg)) e
evalClosure v arg = error $ "[evalClosure] got non-closure " ++ show v 

doApply :: Value -> Value -> Value
doApply fun@(Closure (Abst s t e) ctx) arg = {-myTrace ("[doApply] fun = " ++ show fun ++ ", arg = " ++ show arg) $-} 
    evalClosure fun arg
doApply (Restr _ fun) arg = doApply fun arg
doApply (Neutral f fty) arg = Neutral (App (Neutral f fty) arg) (doApply fty arg)

-- Evaluates nat-induction
doInd :: Value -> Value -> Value -> Value -> Value
doInd ty base step n = case n of
    Zero     -> myTrace ("[doInd] base case") $ base
    Succ n'  -> myTrace ("[doInd] succ case") $ doApply fun prev
        where
            fun = doApply step n
            prev = doInd ty base step n'
    Neutral n Nat -> myTrace ("[doInd] neutral case: n = " ++ show n ++ ", ty = " ++ show ty) $
        Neutral (Ind ty base step n) (doApply ty (Neutral n Nat)) --neutral value
    otherwise -> error $ "[doInd] got " ++ show n

doFst :: Value -> Value
doFst v = case v of
    Pair v1 v2   -> v1
    Neutral x (Closure (Sigma s t e) ctx) -> Neutral (Fst v) (eval ctx t)
    Neutral x (Restr _ cl) -> doFst (Neutral x cl)
    otherwise -> error $ "[doFst] got " ++ show v

doSnd :: Value -> Value
doSnd v = case v of
    Pair v1 v2   -> v2
    Neutral x ty@(Closure (Sigma s t e) ctx) -> Neutral (Snd v) (evalClosure ty x)
    Neutral x (Restr _ cl) -> doSnd (Neutral x cl)
    otherwise -> error $ "[doSnd] got " ++ show v

doComp :: Value -> Value -> Value
doComp fam@(Closure (Abst i I ty) ctx) u@(Sys sys) =
    case eval ctx (App ty (Var i)) of
        {-Closure 
        -}
        Neutral ty' Universe -> myTrace ("[doComp] Neutral " ++ show ty' ++ " U") $
            let sys' = map (\(psi,_) -> (psi,App u (Var i))) sys
            in Neutral (Comp fam u)
                (Closure (Abst i I (Restr sys' (App ty (Var i)))) ctx)

getNeutralType :: Value -> Value
getNeutralType (Neutral _ ty) = ty
getNeutralType v = error $ "[getNeutralType] got non-neutral term " ++ show v

readBack :: [Ident] -> Value -> Term
readBack used v = case v of
    App fun arg -> App (readBack used fun) (readBack used arg)
    Succ v -> Succ (readBack used v)
    Fst v -> Fst (readBack used v)
    Snd v -> Snd (readBack used v)
    Pair v1 v2 -> Pair (readBack used v1) (readBack used v2)
    Sys sys -> Sys (mapElems (readBack used) sys)
    Partial phi ty -> foldPartial phi (readBack used ty)
    Restr sys ty -> foldRestr (mapElems (readBack used) sys) (readBack used ty)
    Ind ty b e n -> Ind (readBack used ty) (readBack used b) (readBack used e) (readBack used n)
    fun@(Closure cl ctx) -> let
        (constr,s,t,e) = extract cl
        used'   = used ++ keys ctx
        s'      = newVar used' s
        t'      = readBack used (eval ctx t)
        fun'    = Closure (constr s t e) (extend ctx s' (Decl t))
        eVal    = evalClosure fun' (Neutral (Var s') (eval ctx t))
        e'      = readBack (s' : used') eVal
        in constr s' t' e'
    Neutral v _ -> readBack used v
    otherwise -> v


normalize :: Ctx -> Term -> Term
normalize ctx e = readBack (keys ctx) (eval ctx e)

{- Linear head reduction -}

headRedV :: Ctx -> Term -> Value
headRedV ctx (Var s) = getLeastEval ctx s
headRedV ctx (App k n) = doApply (headRedV ctx k) (eval ctx n)
headRedV ctx (Ind ty b s k) = doInd (eval ctx ty) (eval ctx b) (eval ctx s) (headRedV ctx k)
headRedV ctx (Fst t) = doFst (eval ctx t)
headRedV ctx (Snd t) = doSnd (eval ctx t)
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
    Sigma s t e -> Sigma s t e'
        where e' = headRed (extend ctx s (Decl t)) e
    Succ t     -> Succ (headRed ctx t)
    Pair t1 t2 -> Pair (headRed ctx t1) (headRed ctx t2)
    otherwise  -> readBack (keys ctx) (headRedV ctx t) 


{- Printing utilities (should be in AbsCTT but these need 'readBack') -}

instance Show Term where
    show t = printTerm' 0 (if debug then t else readBack [] t)

printTerm' :: Int -> Term -> String
printTerm' i t = case t of
    Var s    -> show s
    Universe     -> "U"
    Abst s t e   -> par1 ++ abstS ++ par2
        where abstS = if not (containsVar s e)
                then -- A -> B
                    printTerm' (i+1) t ++ " -> " ++ printTerm' 0 e
                else
                    "[" ++ show s ++ ":" ++ printTerm' 0 t ++ "]" ++ printTerm' 0 e
    Sigma s t e  -> par1 ++ abstS ++ par2
        where abstS = if not (containsVar s e)
                then -- A * B
                    printTerm' (i+1) t ++ " * " ++ printTerm' 0 e
                else
                    "<" ++ show s ++ ":" ++ printTerm' 0 t ++ ">" ++ printTerm' 0 e
    Pair t1 t2 -> par1 ++ printTerm' i t1 ++ "," ++ printTerm' i t2 ++ par2
    Fst t -> par1 ++ printTerm' (i + 1) t ++ ".1" ++ par2
    Snd t -> par1 ++ printTerm' (i + 1) t ++ ".2" ++ par2
    App fun arg -> par1 ++ printTerm' (i+1) inner ++ " " ++ unwords printedArgs ++ par2
        where (inner,args) = collectApps (App fun arg) []
              printedArgs  = map (printTerm' (i+1)) args

    Nat          -> "N"
    Zero         -> "0"
    Succ t       -> par1 ++ "S " ++ printTerm' (i+1) t ++ par2 --if isNum then show (n + 1) else "S" ++ printTerm' (i+1) t
        where (isNum,n) = isNumeral t

    Ind ty b s n -> par1 ++ "ind-N " ++ printTerm' (i+1) ty ++ " " ++ printTerm' (i+1) b ++ " "
         ++ printTerm' (i+1) s ++ " " ++ printTerm' (i+1) n ++ par2
    I            -> "I"
    Sys sys      -> showSystem sys
    Partial phi t-> "[" ++ show phi ++ "]" ++ printTerm' (i+1) t
    Restr sys t  -> showSystem sys ++ printTerm' (i+1) t
    Comp fam u   -> par1 ++ "comp " ++ printTerm' (i+1) fam ++ " " ++ printTerm' (i+1) u ++ par2
    --------
    Closure cl ctx  -> "Cl(" ++ show cl ++ {-"," ++ showCtx ctx ++-} ")"
    Neutral v t  -> printTerm' i v -- "N{" ++ printTerm' i v {- ++ ":" ++ printTerm' (i+1) t -} ++ "}"
    where (par1,par2) = if i == 0 then ("","") else ("(",")")

{-
isVal :: CtxEntry -> Bool
isVal (Val _) = True
isVal _       = False

extend :: Ctx -> Ident -> CtxEntry -> Ctx
extend ctx s e | (isVal e) = case lookup s ctx of
    Just (Val _) -> myTrace ("[###############] duplicated " ++ showEntry (s,e) ++ " in ctx = " ++ showCtx ctx) $ if s == Ident "" then ctx else (s,e) : ctx
    otherwise    -> if s == Ident "" then ctx else (s,e) : ctx
extend ctx s e = if s == Ident "" then ctx else (s,e) : ctx
-}

showCtx :: Ctx -> String
showCtx ctx = "[" ++ intercalate ", " (map showEntry (reverse ctx)) ++ "]"

showOnlyShort :: String -> String
showOnlyShort s = if length s < 0 then "..." else s --debug

showEntry :: (Ident,CtxEntry) -> String
showEntry (s,Decl ty) = show s ++ " : " ++ showOnlyShort (show ty)
showEntry (s,Def ty val) = show s ++ " : " ++ showOnlyShort (show ty) ++ " = " ++ showOnlyShort (show val)
showEntry (s,Val val) = show s ++ " => " ++ show val

showSystem :: System -> String
showSystem sys = "[" ++ intercalate " | " (map (\(ff,t) -> show ff ++ " -> " ++ show t) sys) ++ "]"
