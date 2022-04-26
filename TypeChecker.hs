{-# LANGUAGE FlexibleInstances #-}

module TypeChecker where

import Data.Either
import Data.List (intercalate,find)
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
    Fst p -> do --TODO `p` could have restricted ∑ type
        ty <- inferType ctx dirs p
        case ty of
            c@(Closure (Sigma s t e) ctx1) -> do
                return $ eval ctx1 t
            otherwise -> Left $
                "term '" ++ show t ++ "' has type '" ++ show ty ++ "' , which is not a sum"
    Snd p -> do --TODO `p` could have restricted ∑ type
        ty <- myTrace ("[checkType] Snd " ++ show p) $ inferType ctx dirs p
        case ty of
            c@(Closure (Sigma s t e) ctx1) -> do
                return $ doApply c (doFst $ eval ctx p)
            otherwise -> Left $
                "term '" ++ show t ++ "' has type '" ++ show ty ++ "' , which is not a sum"
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
checkType ctx dirs e v = myTrace ("[checkType]<= e = " ++ show e ++ ", v = " ++ show v ++ ", ctx = " ++ showCtx ctx ++ ", dirs = " ++ show dirs) $ case (e,v) of
    (Abst s t e,Universe) -> do
        checkType ctx dirs t Universe
        checkType (extend ctx s (Decl t)) dirs e Universe
    (Sigma s t e,Universe) -> do
        checkType ctx dirs t Universe
        checkType (extend ctx s (Decl t)) dirs e Universe
    (Abst s t e,Closure (Abst s1 t1 e1) ctx1) -> {-myTrace ("s = " ++ show s ++ ", t = " ++ show t ++ ", e = " ++ show e ++ ", s1 = " ++ show s1 ++ ", t1 = " ++ show t1Val ++ ", e1 = " ++ show e1) $-} do
        checkType ctx dirs t Universe
        let tVal  = eval ctx t
            t1Val = tVal --eval ctx1 t1
        {-unless (conv (keys ctx) tVal t1Val) $
            Left $ "type '" ++ show tVal ++ "' is not convertible to type '" ++ show t1Val ++
                "' (while checking term '" ++ show (Abst s t e) ++ "' against type '" ++ show v ++ "')"-}
        
        let var   = newVar (keys ctx1) s
            e1Val = doApply v (Neutral (Var var) t1Val)
        when (e1Val == I) $
            Left $ "I cannot appear as codomain in products"
        let ctx' = if s == var then
                extend ctx s (Decl t)
            else
                extend (extend ctx s (Decl t)) s (Val (Neutral (Var var) tVal))
        checkType ctx' dirs e e1Val
    (Pair p1 p2,Closure (Sigma s1 t1 e1) ctx1) -> do
        let t1Val = eval ctx1 t1
        checkType ctx dirs p1 t1Val

        let e1Val = doApply v (eval ctx p1)
        when (e1Val == I) $
            Left $ "I cannot appear as codomain in sums"
        --let ctx' = extend (extend ctx s (Decl t)) s (Val (Neutral (Var var) tVal))
        checkType ctx dirs p2 e1Val
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
            myTrace ("[Partial] phi = " ++ show phi ++ ", psi = " ++ show psi) $ unless (impDisj dirs phi psi) $
                Left $ show phi ++ " does not imply " ++ show psi
            unless (conv (keys ctx) dirs ity cty) $
                Left $ "type '" ++ show v ++ "' expected, got term '" ++ show e
                    ++ "' of type '" ++ show ty ++ "' instead"
        else if isPartial ty || isPartial v then do
            let (psi,ity) = splitPartial ty
                (phi,cty) = splitPartial v
            --let msg = getMsg (not $ phi `impDisj` psi) $ ": " ++ show phi ++ " does not imply " ++ show psi
            unless (conv (keys ctx) dirs ity cty && impDisj dirs phi psi) $
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
            myTrace ("sys,ity = " ++ show sys ++ " || " ++ show ity ++ ", csys,cty = " ++ show csys ++ " || " ++ show cty ++ ", phi,psi = " ++ show phi ++ " || " ++ show psi) $
                unless (conv (keys ctx) dirs ity cty && impDisj dirs phi psi &&
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

instance Convertible DisjFormula where
    conv _ dirs disj1 disj2 = eqFormulas dirs disj1 disj2

sameKind :: Term -> Term -> Bool
sameKind (Abst _ _ _) (Abst _ _ _) = True
sameKind (Sigma _ _ _) (Sigma _ _ _) = True
sameKind _ _ = False

instance Convertible Value where
    conv used dirs v1 v2 =
        v1 == v2 || case (v1,v2) of
            (Closure cl1 ctx1,Closure cl2 ctx2) | sameKind cl1 cl2 -> let
                (_,s1,t1,e1) = extract cl1
                (_,s2,t2,e2) = extract cl2
                var = newVar (used ++ keys ctx2) s1
                t1V = eval ctx1 t1
                t2V = eval ctx2 t2
                e1' = doApply v1 (Neutral (Var var) t1V)
                e2' = doApply v2 (Neutral (Var var) t2V)
                in conv used dirs t1V t2V && conv (var : used) dirs e1' e2'
            (Universe,Universe) -> True
            {- Sigma types -}
            (Fst v1,Fst v2) -> conv used dirs v1 v2
            (Snd v1,Snd v2) -> conv used dirs v1 v2
            (Pair v1 v1',Pair v2 v2') -> conv used dirs v1 v1' &&
                conv used dirs v2 v2'
            (v,Pair v1 v2) -> conv used dirs (doFst v) v1 && --TODO: is this `eta`?
                conv used dirs (doSnd v) v2
            (Pair v1 v2,v) -> conv used dirs v1 (doFst v) &&
                conv used dirs v2 (doSnd v)
            {- Naturals -}
            (Nat,Nat)           -> True
            (Zero,Zero)         -> True
            (Succ n1,Succ n2)   -> conv used dirs n1 n2
            {- Cubical -}
            (I,I)               -> True
            (Sys sys1,v2) | isSimplSys dirs sys1 ->
                conv used dirs (simplifySys dirs sys1) v2
            (v1,Sys sys2) | isSimplSys dirs sys2 ->
                conv used dirs v1 (simplifySys dirs sys2)
            (Sys sys1,Sys sys2) -> conv used dirs sys1 sys2
            (Partial phi1 v1,Partial phi2 v2) -> conv used dirs phi1 phi2 &&
                conv used dirs v1 v2
            (Restr sys1 t1,Restr sys2 t2) -> conv used dirs sys1 sys2 &&
                conv used dirs t1 t2
            {- Neutrals -}
            (Var s1,Var s2) -> s1 == s2
            (App f1 a1,App f2 a2) -> conv used dirs f1 f2 && conv used dirs a1 a2 
            (Ind ty1 b1 s1 n1,Ind ty2 b2 s2 n2) ->
                conv used dirs ty1 ty2 && conv used dirs b1 b2 &&
                conv used dirs s1  s2  && conv used dirs n1 n2
            
            (Neutral _ ty1,v2) | isSimpl dirs ty1 ->
                    conv used dirs (simplifyValue dirs ty1) v2
            (v1,Neutral _ ty2) | isSimpl dirs ty2 ->
                    conv used dirs v1 (simplifyValue dirs ty2)
            (Neutral (Var x1) I,Neutral (Var x2) I) -> 
                dirs `makesTrueAtomic` (Diag x1 x2)
            (Neutral v1 _,Neutral v2 _) -> conv used dirs v1 v2
            otherwise             -> False

instance Convertible System where
    conv used dirs sys1 sys2 =
        conv used dirs (getSystemFormula sys1) (getSystemFormula sys2) &&
        all (\(conj,t1,t2) -> convPartialConj used conj dirs t1 t2) meets
            where meets = [(conj1 `meet` conj2, sys1 `at` conj1, sys2 `at` conj2) |
                        conj1 <- keys sys1, conj2 <- keys sys2]

isSimpl :: DirEnv -> Value -> Bool
isSimpl dirs ty = case ty of
    Restr sys _ -> isSimplSys dirs sys
    otherwise   -> False

simplifyValue :: DirEnv -> Value -> Value
simplifyValue dirs (Restr sys _) = simplifySys dirs sys

isSimplSys :: DirEnv -> System -> Bool
isSimplSys dirs sys = any (\(cf,_) -> dirs `makesTrueConj` cf) sys

simplifySys :: DirEnv -> System -> Value
simplifySys dirs sys = snd . fromJust $
    find (\(cf,_) -> dirs `makesTrueConj` cf) sys

