{-# LANGUAGE LambdaCase #-}

module TypeChecker where

import Data.Either
import Data.List (intercalate,find,delete)
import Data.Maybe (fromJust)
import Control.Monad

import Ident
import Interval
import CoreCTT
import Eval


inferType :: Ctx -> DirEnv -> Term -> Either ErrorString Value
inferType ctx dirs t = myTrace ("[inferType]=> t = " ++ show t ++ ", ctx = ..."{- ++ showCtx ctx-}) $ case t of
    Var s -> Right $ lookupType s ctx
    Universe -> Right Universe
    App fun arg -> do
        funTy <- inferType ctx dirs fun

        let (funTy',box) = case funTy of
                Restr sys v -> (v,makeRestr sys)
                otherwise   -> (funTy,curry snd)
            makeRestr :: System -> Value -> Value -> Value
            makeRestr sys val ty = foldRestr (map (\(psi,g) -> (psi,doApply g val)) sys) ty

        case funTy' of
            c@(Closure (Abst s t e) ctx1) -> do
                checkType ctx dirs arg (eval ctx1 t)
                let argVal = eval ctx arg
                return $ box argVal (doApply c argVal)
            otherwise -> Left $
                "term '" ++ show fun ++ "' has type '" ++ show funTy ++ "' , which is not a product"
    Fst p -> do
        ty <- inferType ctx dirs p

        let (ty',box) = case ty of
                Restr sys t -> (t,makeRestr sys)
                otherwise   -> (ty,id)
            makeRestr :: System -> Value -> Value
            makeRestr sys ty = foldRestr (map (\(psi,q) -> (psi,doFst q)) sys) ty

        case ty' of
            c@(Closure (Sigma s t e) ctx1) -> do
                return $ box (eval ctx1 t)
            otherwise -> Left $
                "term '" ++ show t ++ "' has type '" ++ show ty ++ "' , which is not a sum"
    Snd p -> do
        ty <- inferType ctx dirs p

        let (ty',box) = case ty of
                Restr sys t -> (t,makeRestr sys)
                otherwise   -> (ty,id)
            makeRestr :: System -> Value -> Value
            makeRestr sys ty = foldRestr (map (\(psi,q) -> (psi,doSnd q)) sys) ty

        case ty' of
            c@(Closure (Sigma s t e) ctx1) -> do
                return $ box $ evalClosure c (doFst $ eval ctx p)
            otherwise -> Left $
                "term '" ++ show t ++ "' has type '" ++ show ty ++ "' , which is not a sum"
    Split x fam f1 f2 -> do
        ty <- inferType ctx dirs x
        case ty of
            Sum ty1 ty2 -> do
                let ty'@(Sum ty1' ty2') = readBack (keys ctx) ty
                    var = newVar (keys ctx) (Ident "a")
                checkType ctx dirs fam (Closure (Abst (Ident "_") ty' Universe) ctx)
                checkType ctx dirs f1 (eval ctx $ Abst var ty1' (App fam (InL (Var var))))
                checkType ctx dirs f2 (eval ctx $ Abst var ty2' (App fam (InR (Var var))))
                return $ eval ctx (App fam x)
            otherwise -> Left $
                "expected a sum type, got term '" ++ show x ++ "' of type '" ++ show ty ++ "' instead"
    Nat -> Right Universe
    Zero -> Right Nat
    Succ n -> do
        checkType ctx dirs n Nat
        Right Nat 
    Ind ty base step n -> do
        nTyVal <- inferType ctx dirs n
        isNat n nTyVal
        checkType ctx dirs ty (eval emptyCtx (Abst (Ident "") Nat Universe)) -- N -> U

        let tyVal  = eval ctx ty
            tyVal0 = doApply tyVal Zero
        checkType ctx dirs base tyVal0

        let varname = newVar (keys ctx) (Ident "n")
            var     = Var varname
            ctx'    = extend ctx varname (Decl Nat)
        checkType ctx dirs step (eval ctx'
            (Abst varname Nat
                (Abst (Ident "") (App ty var)
                    (App ty (Succ var)))
            )) -- [n : nat] ty n -> ty (suc n)
        let nVal = eval ctx n
        return $ doApply tyVal nVal
    I  -> Right Universe --TODO ?
    I0 -> Right I
    I1 -> Right I
    Comp fam phi@(Disj df) i0 u b i -> do
        checkType ctx dirs fam (eval emptyCtx (Abst (Ident "") I Universe)) -- I -> U
        checkType ctx dirs i0 I
        checkDisjFormula ctx phi
        let var = newVar (keys ctx) (Ident "_i")
        checkType ctx dirs u (eval (extend ctx var (Decl I)) (Abst var I (Partial phi (App fam (Var var)))))
        --checkType ctx dirs u (eval ctx (Partial phi (App fam i)))
        checkType ctx dirs b (eval ctx (App fam i0))
        unless (convPartialDisj (keys ctx) phi dirs (eval ctx b) (eval ctx (App u i0))) . Left $
            "'" ++ show b ++ "' does not agree with '" ++ show (App u i0) ++ "' on " ++ show phi
        let sys = getCompSys phi i0 u b i
        return $ eval ctx $ Restr sys (App fam i)
    _  -> Left $ "don't know how to infer type of '" ++ show t ++ "'"

isNat :: Term -> Value -> Either ErrorString ()
isNat t Nat = Right ()
isNat t v = Left $ "expected type nat, got term '" ++ show t ++
                "' of type '" ++ show v ++ "' instead"

checkTypePartialConj :: ConjFormula -> Ctx -> DirEnv -> Term -> Value -> Either ErrorString ()
checkTypePartialConj conj ctx dirs e v = myTrace ("[checkTypePartialConj] conj = " ++ show conj ++ ", dirs' = " ++ show (addConj dirs conj) ++ ", e = " ++ show e ++ ", v = " ++ show v) $ do
    let dirs' = addConj dirs conj
    unless (inconsistent dirs') $
        checkType ctx dirs' e v

checkTypePartialDisj :: DisjFormula -> Ctx -> DirEnv -> Term -> Value -> Either ErrorString ()
checkTypePartialDisj (Disj df) ctx dirs e v = myTrace ("[checkTypePartialDisj] disj = " ++ show (Disj df) ++ ", e = " ++ show e ++ ", v = " ++ show v) $
    mapM_ (\conj -> checkTypePartialConj conj ctx dirs e v) df

checkType :: Ctx -> DirEnv -> Term -> Value -> Either ErrorString ()
checkType ctx dirs e v = myTrace ("[checkType]<= e = " ++ show e ++ ", v = " ++ show v ++ ", ctx = " ++ showCtx ctx ++ ", dirs = " ++ show dirs) $ case (e,v) of
    (Abst s t e,Universe) -> do
        checkType ctx dirs t Universe
        checkType (extend ctx s (Decl t)) dirs e Universe
    (Sigma s t e,Universe) -> do
        checkType ctx dirs t Universe
        checkType (extend ctx s (Decl t)) dirs e Universe
    (Abst s t e,Closure (Abst s1 t1 e1) ctx1) -> do
        checkType ctx dirs t Universe
        let tVal  = eval ctx t
            t1Val = eval ctx1 t1
        unless (conv (keys ctx) dirs tVal t1Val) $
            Left $ "type '" ++ show tVal ++ "' is not convertible to type '" ++ show t1Val ++
                "' (while checking term '" ++ show (Abst s t e) ++ "' against type '" ++ show v ++ "')"
        
        let var   = newVar (keys ctx1) s
            e1Val = doApply v (Neutral (Var var) t1Val)
        when (e1Val == I) $
            Left "I cannot appear as codomain in products"
        let ctx' = if s == var then
                extend ctx s (Decl t)
            else
                extend (extend ctx s (Decl t)) s (Val (Neutral (Var var) tVal))
        checkType ctx' dirs e e1Val
    (Pair p1 p2,Closure (Sigma s1 t1 e1) ctx1) -> do
        let t1Val = eval ctx1 t1
        checkType ctx dirs p1 t1Val

        let e1Val = evalClosure v (eval ctx p1)
        when (e1Val == I) $
            Left $ "I cannot appear as codomain in sums"
        --let ctx' = extend (extend ctx s (Decl t)) s (Val (Neutral (Var var) tVal))
        checkType ctx dirs p2 e1Val
    (Sum ty1 ty2,Universe) -> do
        checkType ctx dirs ty1 Universe
        checkType ctx dirs ty2 Universe
    (InL t1,Sum ty1 ty2) -> do
        checkType ctx dirs t1 ty1
    (InR t2,Sum ty1 ty2) -> do
        checkType ctx dirs t2 ty2
    (e,Restr sys ty) -> do
        let eVal = eval ctx e
            phi  = getSystemFormula sys
        --checkDisjFormula ctx phi --It should have already been checked, right?
        checkType ctx dirs e ty
        unless (convPartialDisj (keys ctx) phi dirs eVal (Sys sys)) $
            Left $ "term '" ++ show e ++ "' does not agree with '" ++ show (Sys sys) ++ "' on " ++ show phi
    (Sys sys,Partial phi ty) -> myTrace ("sys = " ++ show sys) $ do
        let psis = keys sys
        mapM_ (checkConjFormula ctx) psis
        --unless (impDisj dirs phi (Disj psis)) $
        --    Left $ show phi ++ " does not imply " ++ show (Disj psis)
        unless (eqFormulas dirs phi (Disj psis)) $
            Left $ show phi ++ " is not logically equivalent to " ++ show (Disj psis)

        mapM_ (\(psi,t) -> checkTypePartialConj psi ctx dirs t ty) sys
        let eq_check = all (\((psi1,t1),(psi2,t2)) ->
                convPartialConj (keys ctx) (psi1 `meet` psi2) dirs (eval ctx t1) (eval ctx t2))
                        [(x1,x2) | x1 <- sys, x2 <- sys, x1 /= x2]
        unless eq_check $
            Left $ "values are not adjacent"
    (Partial phi ty,Universe) -> do
        when (eval ctx ty == I) $
            Left "I is not a type"
        checkDisjFormula ctx phi
        checkTypePartialDisj phi ctx dirs ty Universe
    (Restr sys ty,Universe) -> do
        when (eval ctx ty == I) $
            Left "I is not a type"
        checkType ctx dirs ty Universe
        let tyVal = eval ctx ty
            phi   = getSystemFormula sys
        checkDisjFormula ctx phi
        mapM_ (\(conj,t) -> checkTypePartialConj conj ctx dirs t tyVal) sys
    otherwise -> myTrace ("[checkType-otherwise] e = " ++ show e ++ ", v = " ++ show v) $ do
        ty <- inferType ctx dirs e
        unless (compTypes (keys ctx) dirs ty v) $ --check if `ty` is more general than `v`
            Left $ "type '" ++ show v ++ "' expected, got term '" ++ show e
                ++ "' of type '" ++ show ty ++ "' instead"

        -- x : [i = 0]N,  x => [i = 0 \/ i = 1]N
        --      ..v..             ....ty....

compTypes :: [Ident] -> DirEnv -> Value -> Value -> Bool
compTypes used dirs v1 v2 = myTrace ("[compTypes] " ++ show v1 ++ " <~ " ++ show v2 ++ ", dirs = " ++ show dirs) $
    let (iphi,ity) = split v1 --ty
        (vphi,vty) = split v2 --v
        syscheck   = case (v1,v2) of
            (Restr isys _,Restr vsys _) ->
                convPartialDisj used (getSystemFormula vsys) dirs (Sys isys) (Sys vsys)
            otherwise -> True -- ity /= v1 || vty /= v2 --at least one must be a partial type/restriction
        in myTrace ("[compTypes] " ++ show (iphi,ity) ++ " ~? " ++ show (vphi,vty)) $
             syscheck && conv used dirs ity vty && impDisj dirs vphi iphi


checkConjFormula :: Ctx -> ConjFormula -> Either ErrorString ()
checkConjFormula ctx cf = do
    let dom     = keys ctx
        support = vars cf
    unless (all (`elem` dom) support) $
        Left $ "formula '" ++ show cf ++ "' contains undeclared names"

checkDisjFormula :: Ctx -> DisjFormula -> Either ErrorString ()
checkDisjFormula ctx (Disj df) = mapM_ (checkConjFormula ctx) df
