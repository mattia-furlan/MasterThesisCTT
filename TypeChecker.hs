{-# LANGUAGE  FlexibleInstances #-}

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
inferType ctx dirs t = myTrace ("[inferType]>> t = " ++ show t ++ ", ctx = " ++ {-showCtx (ctx)-} "..." ++ ", dirs = " ++ show dirs) $ case t of
    Var s _ -> Right $ lookupType s ctx dirs
        where forceRight eith = case eith of Right r -> r
    Universe -> Right Universe
    App fun arg _ -> do
        funTy <- inferType ctx dirs fun
        case funTy of
            c@(Closure s tVal e _) -> do
                checkType ctx dirs arg tVal
                let argVal = eval ctx dirs arg
                return $ doApply c argVal
            otherwise -> Left $
                "term '" ++ show fun ++ "' has type '" ++ show funTy ++ "' , which is not a product"
    Nat -> Right Universe
    Zero -> Right Nat
    Succ n -> do
        checkType ctx dirs n Nat
        Right Nat 
    Ind ty base step n _ -> do
        nTyVal <- inferType ctx dirs n
        isNat n nTyVal
        checkType ctx dirs ty (eval emptyCtx emptyDirEnv (Abst (Ident "_") Nat Universe))
        let tyVal  = eval ctx dirs ty
            tyVal0 = doApply tyVal Zero
        checkType ctx dirs base tyVal0
        checkType ctx dirs step (eval ctx dirs 
            (Abst (Ident "n") Nat (Abst (Ident "_") (App ty (Var (Ident "n") (Just Nat)) Nothing)
                (App ty (Succ (Var (Ident "n") (Just Nat))) Nothing))))
        let nVal = eval ctx dirs n
        return $ doApply tyVal nVal
    I  -> Right Universe --TODO ?
    I0 -> Right I
    I1 -> Right I 
    _  -> Left $ "don't know how to infer type of '" ++ show t ++ "'"


isNat :: Term -> Value -> Either ErrorString ()
isNat t Nat = Right ()
isNat t v = Left $ "expected type nat, got term '" ++ show t ++
                "' of type '" ++ show v ++ "' instead"

checkTypePartial :: Formula -> Ctx -> DirEnv -> Term -> Value -> Either ErrorString ()
checkTypePartial phi ctx dirs e v = do
    let conjs = toDNFList phi
    mapM_ (\conj -> checkType ctx (addConj dirs conj) e v) conjs

checkType :: Ctx -> DirEnv -> Term -> Value -> Either ErrorString ()
checkType ctx dirs e v = myTrace ("[checkType]>> e = " ++ show e ++ ", v = " ++ show v ++ ", ctx = " ++ {-showCtx (ctx)-} "..." ++ ", dirs = " ++ show dirs) $ case (e,v) of
    (Abst s t e,Universe) -> do
        checkType ctx dirs t Universe
        checkType (extend ctx s (Decl t)) dirs e Universe
    (Abst s t e,Closure s1 t1Val e1 (ctx1,dirs1)) -> {-myTrace ("s = " ++ show s ++ ", t = " ++ show t ++ ", e = " ++ show e ++ ", s1 = " ++ show s1 ++ ", t1 = " ++ show t1Val ++ ", e1 = " ++ show e1) $-} do
        checkType ctx dirs t Universe
        let tVal = eval ctx dirs t
        {-unless (conv (keys ctx) tVal t1Val) $
            Left $ "type '" ++ show tVal ++ "' is not convertible to type '" ++ show t1Val ++
                "' (while checking term '" ++ show (Abst s t e) ++ "' against type '" ++ show v ++ "')"-}
        
        --TODO : I should do something different in the case of an I-abstraction,
        --i.e. I should extend `dirs` using `addISubst` instead of extending the 
        --context using `Var`
        --Now this problem is avoided since I check that no name shadowing occurs,
        --hence in any case `var` = `s`
        let var   = newVar (keys ctx ++ keys ctx1) s
            ctx1' = if s1 == Ident "" then
                    ctx1
                else if s1 == var then
                    extend ctx1 s1 (VDecl t1Val)
                else
                    extend ctx1 s1 (Val (Var var (Just t1Val)))
            e1Val = eval ctx1' dirs1 e1
        when (e1Val == I) $
            Left $ "I cannot appear as codomain in products"
        let ctx' = if s == var then
                extend ctx s (VDecl tVal)
            else
                extend ctx s (Def t (Var var (Just tVal)))
        checkType ctx' dirs e e1Val
    (Sys sys,{-Partial phi ty-}v) -> myTrace (">> sys = " ++ show sys ++ ", v = " ++ show v) $ do
        let (phi,ty) = case v of
                Partial phi ty -> (phi,ty)
                otherwise      -> (FTrue,v)  --TODO is this necessary?
        let phis = map (evalFormula dirs) (keys sys)
        mapM (checkFormula ctx) phis
        unless (conv (keys ctx) (evalFormula dirs $ foldOr phis) (evalFormula dirs phi)) $
            Left $ "formulas don't match"
        mapM (\(phi',t) -> checkTypePartial phi' ctx dirs t ty) sys
        let eq_check = all (\((phi1,t1),(phi2,t2)) ->
                convPartial (keys ctx) dirs (phi1 `makeAnd` phi2) (eval ctx dirs t1) (eval ctx dirs t2))
                        [(x1,x2) | x1 <- sys, x2 <- sys, x1 /= x2]
        unless eq_check $
            Left $ "values are not adjacent"
    (Partial phi ty,Universe) -> do
        --when (conv [] ty I) $
        when (ty == I) $
            Left $ "I is not a type"
        checkTypePartial phi ctx dirs ty Universe
    (Restr sys ty,Universe) -> do
        --when (conv [] ty I) $
        when (ty == I) $
            Left $ "I is not a type"
        checkType ctx dirs ty Universe
        let tyVal = eval ctx dirs ty
            phi   = getSystemFormula sys
        checkTypePartial phi ctx dirs (Sys sys) tyVal
    (e,Restr sys ty) -> do
        let eVal = eval ctx dirs e
            phi = (getSystemFormula sys)
        checkType ctx dirs e ty
        unless (convPartial (keys ctx) dirs phi eVal (Sys sys)) $
            Left $ "term '" ++ show e ++ "' does not agree with '" ++ show sys ++ "' on " ++ show phi
    otherwise -> do
        ty <- inferType ctx dirs e
        let ty' = case ty of
                Restr _ v' -> v'
                otherwise  -> ty
        unless (conv (keys ctx) v ty') $
            Left $ "type '" ++ show v ++ "' expected, got term '" ++ show e
                   ++ "' of type '" ++ show ty ++ "' instead"

checkFormula :: Ctx -> Formula -> Either ErrorString ()
checkFormula ctx ff = do
    let dom     = keys ctx
        support = vars ff
    unless (all (`elem` dom) support) $
        Left $ "formula '" ++ show ff ++ "' contains undeclared names"

class Convertible a where
    conv :: [Ident] -> a -> a -> Bool

convPartial :: [Ident] -> DirEnv -> Formula -> Value -> Value -> Bool
convPartial used dirs ff t1 t2 = all (\conj ->
        conv used (simplifyValue (addConj dirs conj) t1) (simplifyValue (addConj dirs conj) t2)) conjs
    where conjs = toDNFList ff

instance Convertible Formula where
    conv _ ff1 ff2 = equalFormulas ff1 ff2

instance Convertible Value where
    conv used v1 v2 = myTrace ("[conv] v1 = " ++ show v1 ++ ", v2 = " ++ show v2) $  v1 == v2 || case (v1,v2) of
        (Closure s1 t1V e1 (ctx1,dirs1),Closure s2 t2V e2 (ctx2,dirs2)) -> let
            var = newVar (used ++ keys ctx1 ++ keys ctx2) s1
            getNew ctx s tV = let ctx' = extend ctx s (VDecl tV) in
                if var == s then ctx' else extend ctx' s (Val (Var var (Just tV)))
            e1' = eval (getNew ctx1 s1 t1V) dirs1 e1
            e2' = eval (getNew ctx2 s2 t2V) dirs2 e2
            in conv (var : used) t1V t2V && conv (var : used) e1' e2'
        (Universe,Universe) -> True
        {- Naturals -}
        (Nat,Nat)           -> True
        (Zero,Zero)         -> True
        (Succ n1,Succ n2)   -> conv used n1 n2
        {- Cubical -}
        (I,I)               -> True
        (I0,I0)             -> True
        (I1,I1)             -> True
        (Sys sys1,Sys sys2) -> conv used sys1 sys2
        (Partial phi1 v1,Partial phi2 v2) -> conv used phi1 phi2 && conv used v1 v2
        (Restr sys1 t1,Restr sys2 t2) -> conv used sys1 sys2 && conv used t1 t2
        {- Neutrals -}
        (Var s1 _,Var s2 _) -> s1 == s2
        (App fun1 arg1 _,App fun2 arg2 _) -> conv used fun1 fun2 && conv used arg1 arg2
        (Ind ty1 b1 s1 n1 _,Ind ty2 b2 s2 n2 _) ->
            conv used ty1 ty2 && conv used b1  b2 &&
            conv used s1  s2  && conv used n1  n2
        otherwise             -> False

instance Convertible System where
    conv used sys1 sys2 = 
        conv used (foldOr $ keys sys1) (foldOr $ keys sys2) &&
        all (\(phi,t1,t2) -> {-all (\conj ->
            conv used (simplifyValue (toDirEnv conj) t1)
                      (simplifyValue (toDirEnv conj) t2))
            conjs-}

            convPartial used emptyDirEnv phi t1 t2) meets
        where meets = [(phi, sys1 `at` phi1, sys2 `at` phi2) | phi1 <- keys sys1, phi2 <- keys sys2,
                (phi1 `imp` phi2 || phi2 `imp` phi1), phi <- if phi1 `imp` phi2 then [phi1] else [phi2]]


