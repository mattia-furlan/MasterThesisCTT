module Eval where

import Data.List (intercalate)

import Ident
import Interval
import CoreCTT

import Debug.Trace

myTrace :: String -> a -> a
myTrace s = id
--myTrace s x = trace s x

lookupType :: Ident -> Ctx -> DirEnv -> Value
lookupType s ctx dirs = do
    let mentry = lookup s ctx
    case mentry of
        Nothing -> error $ "[lookupType] got unknown identifier " ++ show s
        Just entry -> case entry of
            Decl ty     -> eval ctx dirs ty
            Def  ty def -> eval ctx dirs ty
            VDecl tyV   -> tyV

eval :: Ctx -> DirEnv -> Term -> Value
eval ctx dirs t = myTrace ("[eval] " ++ show t ++ ", dirs = " ++ show dirs) $ case t of
    Var s mty -> case lookupDir s dirs of
        Just i -> toValue i
        Nothing -> case lookup s ctx of
            Nothing -> Var s mty
            Just (Val v)    -> v
            Just (Decl ty)  -> Var s (Just $ eval ctx dirs ty)
            Just (VDecl tyV)  -> Var s (Just tyV)
            Just (Def ty e) -> eval ctx dirs e
    Universe -> Universe
    Abst s t e -> Closure s (eval ctx dirs t) e (ctx,dirs)
    App e1 e2 -> doApply (eval ctx dirs e1) (eval ctx dirs e2)
    Nat -> Nat
    Zero -> Zero
    Succ t -> Succ (eval ctx dirs t)
    Ind ty base step n -> doInd ty' base' step' n'
        where ty'   = eval ctx dirs ty
              base' = eval ctx dirs base
              step' = eval ctx dirs step
              n'    = eval ctx dirs n
    I  -> I
    I0 -> I0
    I1 -> I1
    Sys sys       -> doSystem (evalSystem ctx dirs sys)
    Partial phi t -> doPartial (evalFormula dirs phi) (eval ctx dirs t)
    Restr sys t   -> Restr (evalSystem ctx dirs sys) (eval ctx dirs t)

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

evalSystem :: Ctx -> DirEnv -> System -> System
evalSystem ctx dirs sys = filter (\(phi,_) -> phi /= FFalse) $
    map (\(phi,t) -> (evalFormula dirs phi, eval ctx dirs t)) sys

--Applies substitutions to already formed values (needed when calling `conv`)
simplifyValue :: DirEnv -> Value -> Value
simplifyValue dirs (Var s mty) = case lookupDir s dirs of
    Nothing -> Var s mty
    Just i -> toValue i
simplifyValue dirs (Sys sys) = doSystem $ map (\(phi,v) -> (subst dirs phi,v)) sys
simplifyValue dirs (Restr sys ty) = Restr simplsys ty
    where simplsys = filter (\(phi,_) -> phi /= FFalse) sys'
          sys' = map (\(phi,v) -> (subst dirs phi,v)) sys
simplifyValue dirs (App neutral arg) = neutral @@ (simplifyValue dirs arg)
simplifyValue dirs v = v

-- Evaluates a closure
doApply :: Value -> Value -> Value
doApply (Closure s tVal e (ctx,dirs)) arg = case tVal of
    I -> eval (extend ctx s (Decl I)) (addConj [(s,toInterval arg)] dirs) e
    _ -> eval (extend ctx s (Val arg)) dirs e  -- I don't need to add `t` to `ctx` (?)
doApply neutral arg = neutral @@ arg -- e.g. reduce `p0` to `a` if `p : Path A a b`


(@@) :: Value -> Value -> Value
neutral @@ arg = --myTrace ("[@@] " ++ show neutral ++ " @@ " ++ show arg) $
    case neutral of
        Var s (Just (Closure s' I ty (ctx,dirs))) ->
            let x = doApply (Closure s' I ty (ctx,dirs)) arg in case x of
                Restr [(FTrue,u)] _ -> u
                otherwise           -> App neutral arg
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

doSystem :: System -> Value
doSystem sys = case lookup FTrue sys of
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
readBack used (Restr sys ty) = Restr (mapElems (readBack used) sys) (readBack used ty)
readBack used (Ind ty b e n) = Ind (readBack used ty) (readBack used b) (readBack used e) (readBack used n)
readBack used fun@(Closure s tVal e (ctx,dirs)) = let
        used' = used ++ keys ctx
        s'   = newVar used' s
        t'   = readBack (s' : used') tVal
        eVal = doApply fun (Var s' (Just tVal))
        e'   = readBack (s' : used') eVal
        in Abst s' t' e'
readBack used v = v


normalize :: Ctx -> Term -> Term
normalize ctx e = readBack (keys ctx) (eval ctx emptyDirEnv e)


{- Linear head reduction -}

headRedV :: Ctx -> Term -> Value
headRedV ctx (Var s _) = getLeastEval ctx s
headRedV ctx (App k n) = doApply (headRedV ctx k) (eval ctx emptyDirEnv n)
headRedV ctx (Ind ty b s k) = doInd (eval ctx emptyDirEnv ty) (eval ctx emptyDirEnv b) (eval ctx emptyDirEnv s) (headRedV ctx k)
headRedV ctx t = eval ctx emptyDirEnv t

--Gets the least evaluated form of 'x' from context
getLeastEval :: Ctx -> Ident -> Value
getLeastEval ctx s = case lookup s ctx of
    Just (Def _ e) -> eval ctx emptyDirEnv e
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
    Partial phi t-> "[" ++ show phi ++ "]" ++ printTerm' 0 t
    Restr sys t  -> showSystem sys ++ printTerm' 0 t
    --Closure s tyV e (ctx,dirs)    -> "{" ++ show s ++ "," ++ show e ++ ",ctx=" ++ showCtx ctx ++ ",dirs=" ++ show dirs ++ ")}"
    --Closure s tyV e (ctx,dirs)    -> "{" ++ show s ++ "," ++ show e ++ ")}"
    where (par1,par2) = if i == 0 then ("","") else ("(",")")


showCtx :: Ctx -> String
showCtx ctx = "[" ++ intercalate ", " (map showEntry (reverse ctx)) ++ "]"

showOnlyShort :: String -> String
showOnlyShort s = if length s < 0 then "..." else s

showEntry :: (Ident,CtxEntry) -> String
showEntry (s,Decl ty) = show s ++ " : " ++ showOnlyShort (show ty)
showEntry (s,Def ty val) = show s ++ " : " ++ showOnlyShort (show ty) ++ " = " ++ showOnlyShort (show val)
showEntry (s,VDecl tyV) = show s ++ " :v " ++ show tyV
showEntry (s,Val val) = show s ++ " => " ++ show val

showSystem :: System -> String
showSystem sys = "[" ++ intercalate ", " (map (\(ff,t) -> "(" ++ show ff ++ ") -> " ++ show t) sys) ++ "]"
