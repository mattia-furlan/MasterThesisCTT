{-# LANGUAGE FlexibleInstances #-}

module Conv where

import Data.List (find)
import Data.Maybe (fromJust)

import Ident
import Interval
import CoreCTT
import Eval


-- Generic class for objects that allow a notion of α-conversion
class Convertible a where
    conv :: [Ident] -> DirEnv -> a -> a -> Bool

-- Check convertibility under a conjunctive formula
-- If the formula is false, the values are trivally convertible
convPartialConj :: [Ident] -> ConjFormula -> DirEnv -> Value -> Value -> Bool
--convPartialConj used conj dirs v1 v2 = myTrace ("[convPartialConj] conj = " ++ show conj ++ ", v1 = " ++ show v1 ++ ", v2 = " ++ show v2) $
convPartialConj used conj dirs v1 v2 =
    let dirs' = addConj dirs conj
    in inconsistent dirs' || conv used dirs' v1 v2

-- Two values are convertible under a disjunctive formula iff
-- they are so under each conjunction
convPartialDisj :: [Ident] -> DisjFormula -> DirEnv -> Value -> Value -> Bool
--convPartialDisj used (Disj df) dirs v1 v2 = myTrace ("[convPartialDisj] disj = " ++ show (Disj df) ++ ", v1 = " ++ show v1 ++ ", v2 = " ++ show v2) $
convPartialDisj used (Disj df) dirs v1 v2 =
    all (\conj -> convPartialConj used conj dirs v1 v2) df

-- Check if two ∏/∑-abstractions are of the same kind 
sameKind :: Term -> Term -> Bool
sameKind Abst{}  Abst{}  = True
sameKind Sigma{} Sigma{} = True
sameKind _       _       = False

{- The following functions are used to simplify neutral values
that may become non-neutral under the added constraints in `dirs`.
They are used only during `conv`, and NOT during `eval`. -}

-- Check if a restriction type contains a true formula
-- under the directions environment `dirs`
isSimplRestr :: DirEnv -> Value -> Bool
isSimplRestr dirs ty = case ty of
    Restr sys _ -> isSimplSys dirs sys
    otherwise   -> False

-- Simplify a restriction type if it contains a true formula
-- under the directions environment `dirs`
simplRestr :: DirEnv -> Value -> Value
simplRestr dirs (Restr sys _) = simplSys dirs sys

-- Check if a system contains a true formula
-- under the directions environment `dirs`
isSimplSys :: DirEnv -> System -> Bool
isSimplSys dirs = any ((dirs `makesTrueConj`) . fst)

-- Simplify a system if it contains a true formula
-- under the directions environment `dirs`
simplSys :: DirEnv -> System -> Value
simplSys dirs sys = snd . fromJust $
    find ((dirs `makesTrueConj`) . fst) sys

-- Simplify a system or neutral value if possibile, otherwise do nothing
simpl :: DirEnv -> Value -> Value
simpl dirs (Sys sys)      | isSimplSys dirs sys  = simplSys dirs sys
simpl dirs (Neutral _ ty) | isSimplRestr dirs ty = simplRestr dirs ty
simpl _ v = v

-- Check if the type can be simplified; in that case two values
-- of that type are automatically convertible, because they
-- shall reduce to the same value. That is, we don't need
-- to look inside the terms, i.e. we can ignore the proof
proofIrrelevant :: DirEnv -> Value -> Bool
proofIrrelevant dirs ty = case ty of
    Restr _ ty' -> isSimplRestr dirs ty
        || proofIrrelevant dirs ty'
    Closure cl ctx -> let
        -- Fresh variable to evaluate closures
        varV :: Ident -> Value -> CtxEntry
        varV s t = Val $ Neutral (Var $ newVar (keys ctx) s) (eval ctx t)
        in case cl of
            Abst s t e  -> proofIrrelevant dirs 
                (eval (extend ctx s (varV s t)) e)
            Sigma s t e -> proofIrrelevant dirs (eval ctx t) &&
                proofIrrelevant dirs (eval (extend ctx s (varV s t)) e)
    otherwise -> False

-- αη convertibility for values, which are supposed to have the
-- same type. For efficiency, we first test exact syntactical equality
instance Convertible Value where
    conv used dirs v1 v2 = myTrace ("[conv] " ++ show v1 ++ " ~ " ++ show v2 ++ ", dirs = " ++ show dirs)
--  conv used dirs v1 v2 = 
        v1 == v2 || let cnv = conv used dirs in case (v1,v2) of
            (Universe,Universe) -> True
            -- ∏/∑ closures
            (Closure cl1 ctx1,Closure cl2 ctx2) | sameKind cl1 cl2 -> let
                (_,s1,t1,_) = extract cl1
                (_,_ ,t2,_) = extract cl2
                var = newVar used s1
                t1V = eval ctx1 t1
                t2V = eval ctx2 t2
                e1' = evalClosure v1 (Neutral (Var var) t1V)
                e2' = evalClosure v2 (Neutral (Var var) t2V)
                in cnv t1V t2V && conv (var : used) dirs e1' e2'
            -- η-rule for ∏ (first case)
            (Closure (Abst s1 t1 _) ctx1,Neutral _ (Closure Abst{} _)) -> let
                var = newVar used s1
                t1V = eval ctx1 t1
                e1' = evalClosure v1 (Neutral (Var var) t1V)
                e2' = doApply v2 (Neutral (Var var) t1V)
                in conv (var : used) dirs e1' e2'
            -- η-rule for ∏ (second case)
            (Neutral _ (Closure Abst{} _),Closure (Abst s2 t2 _) ctx2) -> let
                var = newVar used s2
                t2V = eval ctx2 t2
                e1' = doApply (simpl dirs v1) v1 (Neutral (Var var) t2V)
                e2' = evalClosure v2 (Neutral (Var var) t2V)
                in conv (var : used) dirs e1' e2'
            {- Sigma types -}
            (Fst v,Fst v') -> cnv v v'
            (Snd v,Snd v') -> cnv v v'
            (Pair vp1 vp1',Pair vp2 vp2') -> cnv vp1 vp2 &&
                cnv vp1' vp2'
            -- η-rule for ∑ (first case)
            (vp,Pair v v') -> cnv (doFst vp) v &&
                cnv (doSnd vp) v'
            -- η-rule for ∑ (second case)
            (Pair v v',vp) -> cnv v (doFst vp) &&
                cnv v' (doSnd vp)
            {- Coproduct types -}
            (Sum ty1 ty2,Sum ty1' ty2') -> cnv ty1 ty1' && cnv ty2 ty2'
            (InL v,InL v') -> cnv v v'
            (InR v,InR v') -> cnv v v'
            {- Naturals -}
            (Nat,Nat)           -> True
            (Zero,Zero)         -> True
            (Succ n1,Succ n2)   -> cnv n1 n2
            {- Cubical -}
            (I,I)               -> True
            (I0,I0)             -> True
            (I1,I1)             -> True
            -- Systems. We have to check is the system is simplifiable
            -- to avoid an infinite loop
            (Sys sys,_) | isSimplSys dirs sys ->
                cnv (simpl dirs v1) v2
            (_,Sys sys) | isSimplSys dirs sys ->
                cnv v1 (simpl dirs v2)
            (Sys sys,Sys sys') -> conv used dirs sys sys'
            (Partial phi v,Partial phi' v') -> eqFormulas dirs phi phi' &&
                cnv v v'
            (Restr sys t,Restr sys' t') -> conv used dirs sys sys' && cnv t t'
            {- Neutrals -}
            (Var s1,Var s2) -> s1 == s2
            (App f1 a1,App f2 a2) -> cnv f1 f2 && cnv a1 a2 
            (Ind ty1 b1 s1 n1,Ind ty2 b2 s2 n2) ->
                cnv ty1 ty2 && cnv b1 b2 &&
                cnv s1  s2  && cnv n1 n2
            (Split ty1 f1 g1 x1,Split ty2 f2 g2 x2) -> cnv ty1 ty2
                && cnv f1 f2 && cnv g1 g2 && cnv x1 x2
            (Comp fam1 phi1 i01 u1 b1 i1,Comp fam2 phi2 i02 u2 b2 i2) ->
                cnv fam1 fam2 && eqFormulas dirs phi1 phi2 &&
                cnv i01 i02 && cnv u1 u2 && cnv b1 b2 && cnv i1 i2
            -- Neutral values with a simplifiable restriction type
            (Neutral _ ty1,_) | isSimplRestr dirs ty1 ->
                    cnv (simpl dirs v1) v2
            (_,Neutral _ ty2) | isSimplRestr dirs ty2 ->
                    cnv v1 (simpl dirs v2)
            -- Interval names
            (Neutral (Var x1) I,Neutral (Var x2) I) -> 
                dirs `makesTrueAtomic` Diag x1 x2
            (Neutral (Var x1) I,I0) -> 
                dirs `makesTrueAtomic` Eq0 x1
            (Neutral (Var x1) I,I1) -> 
                dirs `makesTrueAtomic` Eq1 x1
            (I0,Neutral (Var x2) I) -> 
                dirs `makesTrueAtomic` Eq0 x2
            (I1,Neutral (Var x2) I) -> 
                dirs `makesTrueAtomic` Eq1 x2
            -- Last case, two neutral values
            -- The types must be convertible, due to type-checking
            (Neutral v ty,Neutral v' _) ->
                proofIrrelevant dirs ty || cnv v v'
            otherwise -> False

-- Convertibility between two systems
instance Convertible System where
    conv used dirs sys1 sys2 =
        eqFormulas dirs (getSystemFormula sys1) (getSystemFormula sys2)
        && all (\(conj,t1,t2) -> convPartialConj used conj dirs t1 t2) meets
        where meets = [(conj1 `meet` conj2, sys1 `at` conj1, sys2 `at` conj2) |
                        conj1 <- keys sys1, conj2 <- keys sys2]