{-# LANGUAGE LambdaCase #-}

module Eval where

import Data.List (intercalate)

import Ident
import Interval
import CoreCTT

import Debug.Trace

myTrace :: String -> a -> a
myTrace s = id
--myTrace s x = trace s x

lookupType :: Ident -> Ctx -> Value
lookupType s ctx = do
    let mentry = lookup s ctx
    case mentry of
        Nothing -> error $ "[lookupType] got unknown identifier " ++ show s
        Just entry -> case entry of
            Decl ty     -> eval ctx ty
            Def  ty def -> eval ctx ty

eval :: Ctx -> Term -> Value
eval ctx t = {-myTrace ("[eval] " ++ show t ++ ", ctx = " ++ showCtx (take 4 ctx)) $-} case t of
    Var s Nothing -> case lookup s ctx of -- `s` must be declared in `ctx`
        Nothing -> error $ "[eval] not found var " ++ show s ++ " in ctx" --Var s mty
        Just (Val v)     -> v
        Just (Decl ty)   -> Var s (Just $ eval ctx ty)
        Just (Def ty e)  -> eval ctx e
    Universe -> Universe
    Abst s t e -> Closure s t e ctx
    App e1 e2 Nothing -> doApply (eval ctx e1) (eval ctx e2)
    Nat -> Nat
    Zero -> Zero
    Succ t -> Succ (eval ctx t)
    Ind ty base step n Nothing -> doInd ty' base' step' n'
        where ty'   = eval ctx ty
              base' = eval ctx base
              step' = eval ctx step
              n'    = eval ctx n
    I  -> I
    I0 -> I0
    I1 -> I1
    Sys sys       -> doSystem (evalSystem ctx sys)
    Partial phi t -> doPartial (evalFormula ctx phi) (eval ctx t)
    Restr sys t   -> Restr (evalSystem ctx sys) (eval ctx t)

toValue :: Interval -> Value
toValue IZero = I0
toValue IOne  = I1
toValue (IVar s) = Var s (Just I)

toInterval :: Value -> Interval
toInterval I0 = IZero
toInterval I1 = IOne
toInterval (Var s _) = IVar s

evalFormula :: Ctx -> Formula -> Formula
evalFormula ctx ff = foldl singleSubst ff substs
    where names  = vars ff
          substs = concatMap (\case (s,Val v) | s `elem` names -> [(s,toInterval v)] ;
                                    otherwise -> []) ctx

evalSystem :: Ctx -> System -> System
evalSystem ctx sys = filter (\(phi,_) -> phi /= FFalse) $
    map (\(phi,t) -> (evalFormula ctx phi, eval ctx t)) sys

--Applies substitutions to already formed values (needed when calling `conv`)
simplifyValue :: DirEnv -> Value -> Value
simplifyValue dirs (Var s mty) = case lookupDir s dirs of
    Nothing -> Var s mty
    Just i  -> toValue i
simplifyValue dirs (Sys sys) = doSystem $ map (\(phi,v) -> (subst dirs phi,v)) sys
simplifyValue dirs (Restr sys ty) = Restr simplsys ty
    where simplsys = filter (\(phi,_) -> phi /= FFalse) sys'
          sys' = map (\(phi,v) -> (subst dirs phi,v)) sys
simplifyValue dirs (App neutral arg nty) = neutral @@ (simplifyValue dirs arg)
simplifyValue dirs v = v

-- Evaluates a closure
doApply :: Value -> Value -> Value
doApply fun@(Closure s t e ctx) arg = 
    let ctx' = if s == Ident "" then ctx
               else extend (extend ctx s (Decl t)) s (Val arg)
    in eval ctx' e  -- I don't need to add `t` to `ctx` (?)
doApply neutral arg = neutral @@ arg -- e.g. reduce `p0` to `a` if `p : Path A a b`

{-
doApply :: Value -> Value -> Value
doApply fun@(Closure s tVal e (ctx,dirs)) arg = myTrace ("[doApply] fun = " ++ show fun ++ ", arg = " ++ show arg) $ case tVal of
    I -> case arg of
        Var s' _ -> eval (extend ctx s (Decl I)) (addISubst dirs s s') e
        _        -> eval (extend ctx s (Decl I)) (addConj dirs [(s,toInterval arg)]) e
    _ -> let ctx' = if s == Ident "" then ctx else (extend (extend ctx s (VDecl tVal)) s (Val arg))
        in eval ctx' dirs e  -- I don't need to add `t` to `ctx` (?)
doApply neutral arg = neutral @@ arg -- e.g. reduce `p0` to `a` if `p : Path A a b`
-}

(@@) :: Value -> Value -> Value
neutral @@ arg = myTrace ("[@@] " ++ show neutral ++ " @@ " ++ show arg) $
    let nty = getNeutralType neutral
        ty  = doApply nty arg
    in case ty of
            Restr [(FTrue,u)] _ -> u
            otherwise           -> App neutral arg (Just ty)

-- Evaluates nat-induction
doInd :: Value -> Value -> Value -> Value -> Value
doInd ty base step n = case n of
    Zero     -> base
    Succ n'  -> doApply fun prev
        where
            fun = doApply step n
            prev = doInd ty base step n'
    otherwise -> Ind ty base step n (Just $ doApply ty n) --neutral value

doSystem :: System -> Value
doSystem sys = case lookup FTrue sys of
    Nothing -> Sys sys
    Just v  -> v

doPartial :: Formula -> Value -> Value
doPartial ff v = case ff of
    FTrue -> v
    _     -> Partial ff v

getNeutralType :: Value -> Value
getNeutralType v | isNeutral v = case v of
    Var _       (Just ty) -> ty
    App _ _     (Just ty) -> ty
    Ind _ _ _ _ (Just ty) -> ty
    otherwise -> error $ "[getNeutralType] got neutral term '" ++ show v ++ "'"

readBack :: [Ident] -> Value -> Term
readBack used (App fun arg _) = App (readBack used fun) (readBack used arg) Nothing
readBack used (Succ v) = Succ (readBack used v)
readBack used (Sys sys) = Sys (mapElems (readBack used) sys)
readBack used (Partial phi ty) = Partial phi (readBack used ty)
readBack used (Restr sys ty) = Restr (mapElems (readBack used) sys) (readBack used ty)
readBack used (Ind ty b e n _) = Ind (readBack used ty) (readBack used b) (readBack used e) (readBack used n) Nothing
readBack used fun@(Closure s t e ctx) = let
        used' = used ++ keys ctx
        s'    = newVar used' s
        t'    = t -- ? -- readBack (s' : used') tVal
        eVal  = doApply fun (Var s' Nothing)
        e'    = readBack (s' : used') eVal
        in Abst s' t' e'
readBack used v = v


normalize :: Ctx -> Term -> Term
normalize ctx e = readBack (keys ctx) (eval ctx e)

{- Linear head reduction -}

headRedV :: Ctx -> Term -> Value
headRedV ctx (Var s _) = getLeastEval ctx s
headRedV ctx (App k n _) = doApply (headRedV ctx k) (eval ctx n)
headRedV ctx (Ind ty b s k _) = doInd (eval ctx ty) (eval ctx b) (eval ctx s) (headRedV ctx k)
headRedV ctx t = eval ctx t

--Gets the least evaluated form of 'x' from context
getLeastEval :: Ctx -> Ident -> Value
getLeastEval ctx s = case lookup s ctx of
    Just (Def _ e) -> eval ctx e
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
    App fun arg mty -> par1 ++ printTerm' (i+1) inner ++ " " ++ intercalate " " printedArgs ++ par2
        where (inner,args) = collectApps (App fun arg mty) []
              printedArgs  = map (printTerm' (i+1)) args
    Nat          -> "N"
    Zero         -> "0"
    Succ t       -> "S" ++ show t --if isNum then show (n + 1) else "S" ++ printTerm' (i+1) t
        where (isNum,n) = isNumeral t

    Ind ty b s n _ -> par1 ++ "ind-N " ++ (printTerm' (i+1) ty) ++ " " ++ (printTerm' (i+1) b) ++ " "
         ++ (printTerm' (i+1) s) ++ " " ++ (printTerm' (i+1) n) ++ par2
    I            -> "I"
    I0           -> "I0"
    I1           -> "I1"
    Sys sys      -> showSystem sys
    Partial phi t-> "[" ++ show phi ++ "]" ++ printTerm' 0 t
    Restr sys t  -> showSystem sys ++ printTerm' 0 t
    --Closure s tyV e ctx    -> "{" ++ show s ++ " : " ++ show tyV ++ "," ++ show e ++ {-"," ++ showCtx ctx ++-} "}"
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
