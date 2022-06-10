{-# LANGUAGE FlexibleInstances #-}
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
    Comp fam phi@(Disj df) i0 u b -> do
        checkType ctx dirs fam (eval emptyCtx (Abst (Ident "") I Universe)) -- I -> U
        checkType ctx dirs i0 I
        checkDisjFormula ctx phi
        let var = newVar (keys ctx) (Ident "i")
        checkType ctx dirs u (eval ctx (Abst var I (Partial phi (App fam (Var var)))))
        checkType ctx dirs b (eval ctx (App fam i0))
        unless (convPartialDisj (keys ctx) phi dirs AlphaEta (eval ctx b) (eval ctx (App u i0))) . Left $
            "'" ++ show b ++ "' does not agree with '" ++ show (App u i0) ++ "' on " ++ show phi
        let sys = getCompSys phi i0 u b var
        --unless (compTypes (keys ctx) dirs (eval (extend ctx var (Decl I)) (Restr sys (App fam (Var var)))) (doApply v (Neutral (Var var) I))) $ Left $
        return $ eval ctx $ Abst var I (Restr sys (App fam (Var var)))
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
            t1Val = tVal --eval ctx1 t1
        {-unless (conv (keys ctx) tVal t1Val) $
            Left $ "type '" ++ show tVal ++ "' is not convertible to type '" ++ show t1Val ++
                "' (while checking term '" ++ show (Abst s t e) ++ "' against type '" ++ show v ++ "')"-}
        
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
    (e,Restr sys ty) -> do
        let eVal = eval ctx e
            phi  = getSystemFormula sys
        --checkDisjFormula ctx phi --It should have already been checked, right?
        checkType ctx dirs e ty
        unless (convPartialDisj (keys ctx) phi dirs AlphaEta eVal (Sys sys)) $
            Left $ "term '" ++ show e ++ "' does not agree with '" ++ show (Sys sys) ++ "' on " ++ show phi
    (Sys sys,Partial phi ty) -> myTrace ("sys = " ++ show sys) $ do
        let psis = keys sys
        mapM_ (checkConjFormula ctx) psis
        unless (impDisj dirs phi (Disj psis)) $
            Left $ show phi ++ " does not imply " ++ show (Disj psis)

        mapM_ (\(psi,t) -> checkTypePartialConj psi ctx dirs t ty) sys
        let eq_check = all (\((psi1,t1),(psi2,t2)) ->
                convPartialConj (keys ctx) (psi1 `meet` psi2) dirs AlphaEta (eval ctx t1) (eval ctx t2))
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
        unless (conv (keys ctx) dirs AlphaEtaSub ty v) $ --check if `ty` is more general than `v`
            Left $ "type '" ++ show v ++ "' expected, got term '" ++ show e
                ++ "' of type '" ++ show ty ++ "' instead"

        -- x : [i = 0]N,  x => [i = 0 \/ i = 1]N
        --      ..v..             ....ty....


checkConjFormula :: Ctx -> ConjFormula -> Either ErrorString ()
checkConjFormula ctx cf = do
    let dom     = keys ctx
        support = vars cf
    unless (all (`elem` dom) support) $
        Left $ "formula '" ++ show cf ++ "' contains undeclared names"

checkDisjFormula :: Ctx -> DisjFormula -> Either ErrorString ()
checkDisjFormula ctx (Disj df) = mapM_ (checkConjFormula ctx) df

data ConvMod = AlphaEta | AlphaEtaSub 
    deriving (Show,Eq)

class Convertible a where
    conv :: [Ident] -> DirEnv -> ConvMod -> a -> a -> Bool

convPartialConj :: [Ident] -> ConjFormula -> DirEnv -> ConvMod -> Value -> Value -> Bool
convPartialConj used conj dirs cmod v1 v2 = myTrace ("[convPartialConj] conj = " ++ show conj ++ ", v1 = " ++ show v1 ++ ", v2 = " ++ show v2) $
    let dirs' = addConj dirs conj
    in inconsistent dirs' || conv used dirs' cmod v1 v2

convPartialDisj :: [Ident] -> DisjFormula -> DirEnv -> ConvMod -> Value -> Value -> Bool
convPartialDisj used (Disj df) dirs cmod v1 v2 = myTrace ("[convPartialDisj] disj = " ++ show (Disj df) ++ ", v1 = " ++ show v1 ++ ", v2 = " ++ show v2) $
    all (\conj -> convPartialConj used conj dirs cmod v1 v2) df

sameKind :: Term -> Term -> Bool
sameKind (Abst {}) (Abst {}) = True
sameKind (Sigma {}) (Sigma {}) = True
sameKind _ _ = False

proofIrrelevant :: DirEnv -> Value -> Bool
proofIrrelevant dirs ty = case ty of
    Restr sys ty' -> any ((dirs `makesTrueConj`) . fst) sys || proofIrrelevant dirs ty'
    Closure cl ctx -> case cl of
        Abst s t e  -> proofIrrelevant dirs (eval (extend ctx s (Val . Var $ newVar (keys ctx) s)) e)
        Sigma s t e -> proofIrrelevant dirs (eval ctx t) &&
            proofIrrelevant dirs (eval (extend ctx s (Val . Var $ newVar (keys ctx) s)) e)
    otherwise -> False

instance Convertible Value where
    conv used dirs cmod v1 v2 = myTrace ("[conv-" ++ show cmod ++ "] " ++ show v1 ++ " ~ " ++ show v2 ++ ", dirs = " ++ show dirs)
        v1 == v2 || let cnv = conv used dirs cmod in case (v1,v2) of
            (Universe,Universe) -> True
            (Closure cl1 ctx1,Closure cl2 ctx2) | sameKind cl1 cl2 -> let
                (_,s1,t1,e1) = extract cl1
                (_,s2,t2,e2) = extract cl2
                var = newVar (used ++ keys ctx2) s1
                t1V = eval ctx1 t1
                t2V = eval ctx2 t2
                e1' = evalClosure v1 (Neutral (Var var) t1V)
                e2' = evalClosure v2 (Neutral (Var var) t2V)
                in cnv t1V t2V && conv (var : used) dirs cmod e1' e2'
            (Closure (Abst s1 t1 e1) ctx1,v2@(Neutral _ (Closure (Abst _ _ _) _))) -> let
                var = newVar used s1
                t1V = eval ctx1 t1
                e1' = evalClosure v1 (Neutral (Var var) t1V)
                e2' = doApply (simpl dirs v2) (Neutral (Var var) t1V)
                in conv (var : used) dirs cmod e1' e2'
            (v1@(Neutral _ (Closure (Abst _ _ _) _)),Closure (Abst s2 t2 e2) ctx2) -> let
                var = newVar used s2
                t2V = eval ctx2 t2
                e2' = evalClosure v2 (Neutral (Var var) t2V)
                e1' = doApply (simpl dirs v1) (Neutral (Var var) t2V)
                in conv (var : used) dirs cmod e1' e2'
            {- Sigma types -}
            (Fst v1,Fst v2) -> cnv v1 v2
            (Snd v1,Snd v2) -> cnv v1 v2
            (Pair v1 v1',Pair v2 v2') -> cnv v1 v1' &&
                cnv v2 v2'
            (v,Pair v1 v2) -> cnv (doFst $ simpl dirs v) v1 &&
                cnv (doSnd $ simpl dirs v) v2
            (Pair v1 v2,v) -> cnv v1 (doFst $ simpl dirs v) &&
                cnv v2 (doSnd $ simpl dirs v)
            {- Naturals -}
            (Nat,Nat)           -> True
            (Zero,Zero)         -> True
            (Succ n1,Succ n2)   -> cnv n1 n2
            {- Cubical -}
            (I,I)               -> True
            (I0,I0)             -> True
            (I1,I1)             -> True
            (Sys sys1,v2) | isSimplSys dirs sys1 ->
                cnv (simpl dirs v1) v2
            (v1,Sys sys2) | isSimplSys dirs sys2 ->
                cnv v1 (simpl dirs v2)
            (Sys sys1,Sys sys2) -> conv used dirs cmod sys1 sys2
            {-(Comp fam1 phi1 i1 u1 b1,v2) | dirs `makesTrueDisj` phi1 -> --TODO.. not needed!?
                cnv u1 v2
            (v1,Comp fam2 phi2 i2 u2 b2) | dirs `makesTrueDisj` phi2 ->
                cnv v1 u2-}
            (Partial phi1 v1,Partial phi2 v2) | cmod /= AlphaEtaSub -> eqFormulas dirs phi1 phi2 &&
                cnv v1 v2
            -- (Restr sys1 t1,Restr sys2 t2) | cmod /= AlphaEtaSub -> conv used dirs cmod sys1 sys2 &&
            --     cnv t1 t2
            (Restr sys1 t1,Restr sys2 t2) | cmod /= AlphaEtaSub -> cnv t1 t2 &&
                convPartialDisj used (getSystemFormula sys1) dirs cmod (Sys sys1) (Sys sys2) &&
                convPartialDisj used (getSystemFormula sys2) dirs cmod (Sys sys2) (Sys sys1)
            {- Neutrals -}
            (Var s1,Var s2) -> s1 == s2
            (App f1 a1,App f2 a2) -> cnv f1 f2 && cnv a1 a2 
            (Ind ty1 b1 s1 n1,Ind ty2 b2 s2 n2) ->
                cnv ty1 ty2 && cnv b1 b2 &&
                cnv s1  s2  && cnv n1 n2
            (Comp fam1 phi1 i1 u1 b1,Comp fam2 phi2 i2 u2 b2) ->
                cnv fam1 fam2 && eqFormulas dirs phi1 phi2 &&
                cnv i1 i2 && cnv u1 u2 && cnv b1 b2
            (Neutral _ ty1,v2) | isSimplRestr dirs ty1 ->
                    cnv (simplifyRestr dirs ty1) v2
            (v1,Neutral _ ty2) | isSimplRestr dirs ty2 ->
                    cnv v1 (simplifyRestr dirs ty2)
            (Neutral (Var x1) I,Neutral (Var x2) I) -> 
                dirs `makesTrueAtomic` (Diag x1 x2)
            (Neutral (Var x1) I,I0) -> 
                dirs `makesTrueAtomic` (Eq0 x1)
            (Neutral (Var x1) I,I1) -> 
                dirs `makesTrueAtomic` (Eq1 x1)
            (I0,Neutral (Var x2) I) -> 
                dirs `makesTrueAtomic` (Eq0 x2)
            (I1,Neutral (Var x2) I) -> 
                dirs `makesTrueAtomic` (Eq1 x2)
            (Neutral v1 ty1,Neutral v2 ty2) -> 
                proofIrrelevant dirs ty1 || cnv v1 v2
            otherwise -> cmod == AlphaEtaSub &&     -- x : [i = 0]N,  x => [i = 0 \/ i = 1]N                      
                let (iphi,ity) = split v1 --ty      --      ..v..             ....ty....
                    (vphi,vty) = split v2 --v
                    syscheck   = case (v1,v2) of
                        (Restr isys _,Restr vsys _) ->
                            convPartialDisj used (getSystemFormula vsys) dirs AlphaEta (Sys isys) (Sys vsys)
                        otherwise -> ity /= v1 || vty /= v2 --at least one must be a partial type/restriction
                    in myTrace ("[compTypes] " ++ show (iphi,ity) ++ " ~? " ++ show (vphi,vty)) $
                         syscheck && conv used dirs AlphaEtaSub ity vty && impDisj dirs vphi iphi --TODO

instance Convertible System where
    conv used dirs cmod sys1 sys2 =
        eqFormulas dirs (getSystemFormula sys1) (getSystemFormula sys2) &&
        all (\(conj,t1,t2) -> convPartialConj used conj dirs cmod t1 t2) meets
            where meets = [(conj1 `meet` conj2, sys1 `at` conj1, sys2 `at` conj2) |
                        conj1 <- keys sys1, conj2 <- keys sys2]



