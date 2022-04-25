{-# LANGUAGE FlexibleInstances #-}

module TypeChecker where

import Data.Either
import Data.List (intercalate)
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Monad

import Ident
import Interval
import CoreCTT
import Eval


inferType :: Ctx -> DirEnv -> Term -> Either ErrorString Value
inferType ctx dirs t = myTrace ("[inferType]>> t = " ++ show t ++ ", ctx = " ++ showCtx ctx) $ case t of
    Var s Nothing -> Right $ lookupType s ctx
    Universe -> Right Universe
    App fun arg Nothing -> do
        funTy <- inferType ctx dirs fun
        let funTy' = case funTy of --TODO new rules with restriction function types?
                Restr _ v -> v
                otherwise -> funTy
        case funTy' of
            c@(Closure s t e ctx1) -> do
                checkType ctx dirs arg (eval ctx1 t)
                let argVal = eval ctx arg
                return $ doApply c argVal
            otherwise -> Left $
                "term '" ++ show fun ++ "' has type '" ++ show funTy ++ "' , which is not a product"
    Nat -> Right Universe
    Zero -> Right Nat
    Succ n -> do
        checkType ctx dirs n Nat
        Right Nat 
    Ind ty base step n Nothing -> do
        nTyVal <- inferType ctx dirs n
        isNat n nTyVal
        checkType ctx dirs ty (eval emptyCtx (Abst (Ident "") Nat Universe)) -- N -> U

        let tyVal  = eval ctx ty
            tyVal0 = doApply tyVal Zero
        checkType ctx dirs base tyVal0

        let varname = newVar (keys ctx) (Ident "n")
            var     = Var varname Nothing
            ctx'    = extend ctx varname (Decl Nat)
        checkType ctx dirs step (eval ctx'
            (Abst varname Nat
                (Abst (Ident "") (App ty var Nothing)
                    (App ty (Succ var) Nothing))
            )) -- (n : nat) ty n -> ty (suc n)
        let nVal = eval ctx n
        return $ doApply tyVal nVal
    I  -> Right Universe --TODO ?
    _  -> Left $ "don't know how to infer type of '" ++ show t ++ "'"


isNat :: Term -> Value -> Either ErrorString ()
isNat t Nat = Right ()
isNat t v = Left $ "expected type nat, got term '" ++ show t ++
                "' of type '" ++ show v ++ "' instead"

checkTypePartialConj :: ConjFormula -> Ctx -> DirEnv -> Term -> Value -> Either ErrorString ()
checkTypePartialConj conj ctx dirs e v = myTrace ("[checkTypePartialConj] conj = " ++ show conj ++ ", dirs' = " ++ show (addConj dirs conj) ++ ", e = " ++ show e ++ ", v = " ++ show v) $ do
    let dirs' = addConj dirs conj
    if not (inconsistent dirs') then
        checkType ctx dirs' e v
    else
        return ()

checkTypePartialDisj :: DisjFormula -> Ctx -> DirEnv -> Term -> Value -> Either ErrorString ()
checkTypePartialDisj (Disj df) ctx dirs e v = myTrace ("[checkTypePartialDisj] disj = " ++ show (Disj df) ++ ", e = " ++ show e ++ ", v = " ++ show v) $
    mapM_ (\conj -> checkTypePartialConj conj ctx dirs e v) df

checkType :: Ctx -> DirEnv -> Term -> Value -> Either ErrorString ()
checkType ctx dirs e v = myTrace ("[checkType]>> e = " ++ show e ++ ", v = " ++ show v ++ ", ctx = " ++ showCtx ctx ++ ", dirs = " ++ show dirs) $ case (e,v) of
    (Abst s t e,Universe) -> do
        checkType ctx dirs t Universe
        checkType (extend ctx s (Decl t)) dirs e Universe
    (Abst s t e,Closure s1 t1 e1 ctx1) -> {-myTrace ("s = " ++ show s ++ ", t = " ++ show t ++ ", e = " ++ show e ++ ", s1 = " ++ show s1 ++ ", t1 = " ++ show t1Val ++ ", e1 = " ++ show e1) $-} do
        checkType ctx dirs t Universe
        let tVal  = eval ctx t
            t1Val = tVal --eval ctx1 t1
        {-unless (conv (keys ctx) tVal t1Val) $
            Left $ "type '" ++ show tVal ++ "' is not convertible to type '" ++ show t1Val ++
                "' (while checking term '" ++ show (Abst s t e) ++ "' against type '" ++ show v ++ "')"-}
        
        let var   = newVar (keys ctx1) s
            e1Val = doApply v (Var var (Just t1Val))
        when (e1Val == I) $
            Left $ "I cannot appear as codomain in products"
        let ctx' = if s == var then
                extend ctx s (Decl t)
            else
                extend (extend ctx s (Decl t)) s (Val (Var var (Just tVal)))
        checkType ctx' dirs e e1Val
    (e,Restr sys ty) -> do
        let eVal = eval ctx e
            phi  = getSystemFormula sys
        --checkDisjFormula ctx phi --It should already be checked, right?
        checkType ctx dirs e ty
        unless (convPartialDisj (keys ctx) phi dirs eVal (Sys sys)) $
            Left $ "term '" ++ show e ++ "' does not agree with '" ++ show sys ++ "' on " ++ show phi
    (Sys sys,v) -> myTrace (">> sys = " ++ showSystem sys ++ ", v = " ++ show v) $ do
        let (phi,ty) = case v of
                Partial phi ty -> (phi,ty)
                --otherwise      -> (fTrue,v)  --TODO is this necessary?
        let psis = keys sys
        mapM (checkConjFormula ctx) psis
        unless (conv [] dirs (Disj psis) phi) $
            Left $ "formulas don't match: got " ++ show (Disj psis) ++ " and " ++ show phi
        mapM (\(psi,t) -> checkTypePartialConj psi ctx dirs t ty) sys
        let eq_check = all (\((psi1,t1),(psi2,t2)) ->
                convPartialConj (keys ctx) (psi1 `meet` psi2) dirs (eval ctx t1) (eval ctx t2))
                        [(x1,x2) | x1 <- sys, x2 <- sys, x1 /= x2]
        unless eq_check $
            Left $ "values are not adjacent"
    (Partial phi ty,Universe) -> do
        --when (conv [] ty I) $
        when (eval ctx ty == I) $
            Left $ "I is not a type"
        checkDisjFormula ctx phi
        checkTypePartialDisj phi ctx dirs ty Universe
    (Restr sys ty,Universe) -> do
        --when (conv [] ty I) $
        when (eval ctx ty == I) $
            Left $ "I is not a type"
        checkType ctx dirs ty Universe
        let tyVal = eval ctx ty
            phi   = getSystemFormula sys
        checkDisjFormula ctx phi
        mapM_ (\(conj,t) -> checkTypePartialConj conj ctx dirs t tyVal) sys
    otherwise -> do
        ty <- inferType ctx dirs e
        if isPartial v && isRestr ty then do
            let (sys,ity) = splitRestr ty
                (phi,cty) = splitPartial v
                psi = getSystemFormula sys
            myTrace ("[Partial] phi = " ++ show phi ++ ", psi = " ++ show psi) $ unless (phi `impDisj` psi) $
                Left $ show phi ++ " does not imply " ++ show psi
            unless (conv (keys ctx) dirs ity cty) $
                Left $ "type '" ++ show v ++ "' expected, got term '" ++ show e
                    ++ "' of type '" ++ show ty ++ "' instead"
        else if isPartial ty || isPartial v then do
            let (psi,ity) = splitPartial ty
                (phi,cty) = splitPartial v
            --let msg = getMsg (not $ phi `impDisj` psi) $ ": " ++ show phi ++ " does not imply " ++ show psi
            unless (conv (keys ctx) dirs ity cty && phi `impDisj` psi) $
                Left $ "type '" ++ show v ++ "' expected, got term '" ++ show e
                    ++ "' of type '" ++ show ty ++ "' instead"-- ++ msg
        else if isRestr ty || isRestr v then do
            let (sys,ity)  = splitRestr ty
                (csys,cty) = splitRestr v
                psi = getSystemFormula sys
                phi = getSystemFormula csys
            --myTrace ("[Partial] phi = " ++ show phi ++ ", psi = " ++ show psi) $ unless (phi `impDisj` psi) $
            --    Left $ show phi ++ " does not imply " ++ show psi
            --unless (convPartialDisj (keys ctx) phi dirs (Sys sys) (Sys csys)) $
            --    Left $ "incompatible systems" 
            unless (conv (keys ctx) dirs ity cty && phi `impDisj` psi &&
                convPartialDisj (keys ctx) phi dirs (Sys sys) (Sys csys)) $
                Left $ "type '" ++ show v ++ "' expected, got term '" ++ show e
                    ++ "' of type '" ++ show ty ++ "' instead"
        else unless (conv (keys ctx) dirs v ty) $
            Left $ "type '" ++ show v ++ "' expected, got term '" ++ show e
                ++ "' of type '" ++ show ty ++ "' instead"

checkConjFormula :: Ctx -> ConjFormula -> Either ErrorString ()
checkConjFormula ctx cf = do
    let dom     = keys ctx
        support = vars cf
    unless (all (`elem` dom) support) $
        Left $ "formula '" ++ show cf ++ "' contains undeclared names"

checkDisjFormula :: Ctx -> DisjFormula -> Either ErrorString ()
checkDisjFormula ctx (Disj df) = mapM_ (checkConjFormula ctx) df

class Convertible a where
    conv :: [Ident] -> DirEnv -> a -> a -> Bool

convPartialConj :: [Ident] -> ConjFormula -> DirEnv -> Value -> Value -> Bool
convPartialConj used conj dirs v1 v2 = myTrace ("[convPartialConj] conj = " ++ show conj ++ ", v1 = " ++ show v1 ++ ", v2 = " ++ show v2) $
    let dirs' = addConj dirs conj
    in inconsistent dirs' || conv used dirs' v1 v2

convPartialDisj :: [Ident] -> DisjFormula -> DirEnv -> Value -> Value -> Bool
convPartialDisj used (Disj df) dirs v1 v2 = myTrace ("[convPartialDisj] disj = " ++ show (Disj df) ++ ", v1 = " ++ show v1 ++ ", v2 = " ++ show v2) $
    all (\conj -> convPartialConj used conj dirs v1 v2) df

--instance Convertible DisjFormula where
--    conv _ _ df1 df2 = df1 `impDisj` df2 && df2 `impDisj` df1
convFormulas :: DisjFormula -> DisjFormula -> Bool
convFormulas disj1 disj2 = disj1 `impDisj` disj2 && disj2 `impDisj` disj1

instance Convertible DisjFormula where
    conv _ dirs disj1 disj2 =
        convFormulas disj1 disj2
        {-convFormulas (simplifyDisjFormula dirs disj1)
                     (simplifyDisjFormula dirs disj2)-}

instance Convertible Value where
    conv used dirs v1 v2 = myTrace ("[conv] v1 = " ++ show v1 ++ ", v2 = " ++ show v2 ++ ", dirs = " ++ show dirs) $
        v1 == v2 || case (v1,v2) of
            (Closure s1 t1 e1 ctx1,Closure s2 t2 e2 ctx2) -> let
                var = newVar (used ++ keys ctx2) s1
                getNew ctx s t tV = let ctx' = extend ctx s (Decl t) in
                    if var == s1 then ctx' else extend ctx' s (Val (Var var (Just tV)))
                t1V = eval ctx1 t1
                t2V = eval ctx2 t2
                e1' = eval (getNew ctx1 s1 t1 t1V) e1
                e2' = eval (getNew ctx2 s2 t2 t2V) e2
                in conv (var : used) dirs t1V t2V && conv (var : used) dirs e1' e2'
            (Universe,Universe) -> True
            {- Naturals -}
            (Nat,Nat)           -> True
            (Zero,Zero)         -> True
            (Succ n1,Succ n2)   -> conv used dirs n1 n2
            {- Cubical -}
            (I,I)               -> True
            (Sys sys1,Sys sys2) -> 
                if isSimpl dirs v1 || isSimpl dirs v2 then
                    conv used dirs (simplifyValue dirs v1) (simplifyValue dirs v2)
                else
                    conv used dirs sys1 sys2
            (Sys _,_) ->
                isSimpl dirs v1 && conv used dirs (simplifyValue dirs v1) v2
            (_,Sys _) ->
                isSimpl dirs v2 && conv used dirs v1 (simplifyValue dirs v2)
            (Partial phi1 v1,Partial phi2 v2) -> conv used dirs phi1 phi2 &&
                conv used dirs v1 v2
            (Restr sys1 t1,Restr sys2 t2) -> conv used dirs sys1 sys2 &&
                conv used dirs t1 t2
            {- Neutrals -}
            (Var s1 _,Var s2 _) -> s1 == s2
            (App fun1 arg1 _,App fun2 arg2 _) ->
                if isSimpl dirs v1 || isSimpl dirs v2 then
                    conv used dirs (simplifyValue dirs v1) (simplifyValue dirs v2)
                else
                    conv used dirs fun1 fun2 && conv used dirs arg1 arg2
            (App _ _ _,_) ->
                isSimpl dirs v1 && conv used dirs (simplifyValue dirs v1) v2
            (_,App _ _ _) ->
                isSimpl dirs v2 && conv used dirs v1 (simplifyValue dirs v2)
            (Ind ty1 b1 s1 n1 _,Ind ty2 b2 s2 n2 _) ->
                conv used dirs ty1 ty2 && conv used dirs b1 b2 &&
                conv used dirs s1  s2  && conv used dirs n1 n2
            otherwise             -> False

instance Convertible System where
    conv used dirs sys1 sys2 =
        conv used dirs (getSystemFormula sys1) (getSystemFormula sys2) &&
        all (\(conj,t1,t2) -> convPartialConj used conj dirs t1 t2) meets
            where meets = [(conj1 `meet` conj2, sys1 `at` conj1, sys2 `at` conj2) |
                        conj1 <- keys sys1, conj2 <- keys sys2]

isSimpl :: DirEnv -> Value -> Bool
isSimpl dirs v = case v of
    App nfun arg (Just ty) -> case ty of
        Restr sys _ -> isSimpl dirs (Sys sys)
        otherwise   -> False
    Sys sys -> any (\(cf,_) -> dirs `makesTrue` cf) sys

simplifyValue :: DirEnv -> Value -> Value
simplifyValue dirs v = myTrace ("[simplifyValue] v = " ++ show v ++ ", dirs = " ++ show dirs) $ case v of
    App nfun arg (Just ty) -> case ty of
        Restr sys _ -> case simplifyValue dirs (Sys sys) of
            Sys _ -> v
            u     -> u
        otherwise   -> v
    Sys sys -> let found = filter (\(cf,_) -> dirs `makesTrue` cf) sys
        in case found of
            [] -> Sys sys
            _  -> snd $ head found

