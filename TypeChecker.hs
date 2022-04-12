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

import Debug.Trace

inferType :: Ctx -> DirEnv -> Term -> Either ErrorString Value
inferType ctx dirs t = case t of
    Var s _ -> Right $ eval (ctxToEnv ctx ++ toEnv dirs) (lookupType ctx s)
        where forceRight eith = case eith of Right r -> r
    Universe -> Right Universe
    App fun arg -> do
        funTy <- inferType ctx dirs fun
        case funTy of
            c@(Closure s tVal e _) -> do
                checkType ctx dirs arg tVal
                let argVal = eval (ctxToEnv ctx ++ toEnv dirs) arg
                return $ doApply c argVal
            otherwise -> Left $
                "term '" ++ show fun ++ "' has type '" ++ show funTy ++ "' , which is not a product"
    Nat -> Right Universe
    Zero -> Right Nat
    Succ n -> do
        checkType ctx dirs n Nat
        Right Nat 
    Ind ty base step n -> do
        nTyVal <- inferType ctx dirs n
        isNat n nTyVal
        checkType ctx dirs ty (eval emptyEnv (Abst (Ident "_") Nat Universe))
        let tyVal  = eval (ctxToEnv ctx ++ toEnv dirs) ty
            tyVal0 = doApply tyVal Zero
        checkType ctx dirs base tyVal0
        checkType ctx dirs step (eval (ctxToEnv ctx ++ toEnv dirs)
            (Abst (Ident "n") Nat (Abst (Ident "_") (App ty (Var (Ident "n") (Just Nat)))
                (App ty (Succ (Var (Ident "n") (Just Nat)))))))
        let nVal = eval (ctxToEnv ctx ++ toEnv dirs) n
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
    mapM_ (\conj -> checkType ctx (addConj conj dirs) e v) conjs

checkType :: Ctx -> DirEnv -> Term -> Value -> Either ErrorString ()
checkType ctx dirs e v = {-trace ("e = " ++ show e ++ ", v = " ++ show v ++ ", ctx = " ++ showCtx ctx ++ ", dirs = " ++ show dirs) $-} case (e,v) of
    (Abst s t e,Universe) -> do
        checkType ctx dirs t Universe
        checkType (extend ctx s (Decl t)) dirs e Universe
    (Abst s t e,Closure s1 t1Val e1 rho) -> {-trace ("s = " ++ show s ++ ", t = " ++ show t ++ ", e = " ++ show e ++ ", s1 = " ++ show s1 ++ ", t1 = " ++ show t1Val ++ ", e1 = " ++ show e1) $-} do
        when (conv dirs e1 I) $
            Left $ "I cannot appear as codomain in products"
        checkType ctx dirs t Universe
        let tVal = eval (ctxToEnv ctx ++ toEnv dirs) t
        unless (conv dirs tVal t1Val) $
            Left $ "type '" ++ show tVal ++ "' is not convertible to type '" ++ show t1Val ++
                "' (while checking term '" ++ show (Abst s t e) ++ "' against type '" ++ show v ++ "')"
        --Instead of 's' I should choose a new name ('s' could be already in the context)
        let var = newVar (keys ctx ++ [s,s1]) s
        let e1Val = eval (extend rho s1 (Val (Var var (Just t1Val))) ++ toEnv dirs) e1
        {-trace ("e1Val = " ++ show e1Val ++ ", var = " ++ show var ++ ", rhoE1 = " ++ showEnv (extend rho s1 (Val (Var var)) ++ toEnv dirs)) $ -}
        --checkType (extend ctx s (Def t (Var var))) dirs e e1Val
        checkType (extend (extend ctx var (Decl t)) s (Def t (Var var (Just tVal)))) dirs e e1Val
    (Sys sys,{-Partial phi ty-}v) -> {-trace ("sys = " ++ show sys ++ ", phi = " ++ show phi ++ ", ty = " ++ show ty) $-} do
        let (phi,ty) = case v of
                Partial phi ty -> (phi,ty)
                otherwise      -> (FTrue,v)  --TODO is this necessary?
        let phis = map (evalFormula (ctxToEnv ctx ++ toEnv dirs)) (keys sys)
        mapM_ (checkFormula ctx) phis
        unless (conv dirs (foldOr phis) phi) $ --TODO
            Left $ "formulas don't match"
        mapM (\(phi',t) -> checkTypePartial phi' ctx dirs t ty) sys
        let eq_check = all (\((phi1,t1),(phi2,t2)) -> convPartial (phi1 `makeAnd` phi2) t1 t2)
                        [(x1,x2) | x1 <- sys, x2 <- sys, x1 /= x2]
        unless eq_check $
            Left $ "values are not adjacent"
    (Partial phi ty,Universe) -> do
        when (conv dirs ty I) $
            Left $ "I is not a type"
        checkTypePartial phi ctx dirs ty Universe
    (Restr phi u ty,Universe) -> do
        when (conv dirs ty I) $
            Left $ "I is not a type"
        checkType ctx dirs ty Universe
        let tyVal = eval (ctxToEnv ctx ++ toEnv dirs) ty
        checkTypePartial phi ctx dirs u tyVal
    (e,Restr phi u ty) -> do
        let eVal = eval (ctxToEnv ctx ++ toEnv dirs) e
        checkType ctx dirs e ty
        unless (convPartial phi eVal u) $
            Left $ "term '" ++ show eVal ++ "' does not agree with '" ++ show u ++ "' on " ++ show phi
    otherwise -> do
        ty <- inferType ctx dirs e
        unless (conv dirs v ty) $
            Left $ "type '" ++ show v ++ "' expected, got term '" ++ show e
                   ++ "' of type '" ++ show ty ++ "' instead"


checkFormula :: Ctx -> Formula -> Either ErrorString ()
checkFormula ctx ff = do
    let dom     = keys ctx
        support = vars ff
    unless (all (`elem` dom) support) $
        Left $ "formula '" ++ show ff ++ "' contains undeclared names"

class Convertible a where
    conv' :: (Int, Map Ident Int, Map Ident Int) -> DirEnv -> a -> a -> Bool

    conv :: DirEnv -> a -> a -> Bool
    conv dirs = conv' (0, Map.empty, Map.empty) dirs

convPartial :: (Convertible a,Show a) => Formula -> a -> a -> Bool
convPartial ff t1 t2 = all (\conj -> trace ("conj = " ++ show conj) $ conv (toDirEnv conj) t1 t2) conjs
    where conjs = toDNFList ff

instance Convertible Formula where
    conv' _ dirs ff1 ff2 = equalFormulas (substDirs ff1 dirs) (substDirs ff2 dirs)

instance Convertible Term where
    conv' info@(i,env1,env2) dirs v1 v2 = v1 == v2 || case (v1,v2) of
        (Closure s1 t1 e1 rho1,Closure s2 t2 e2 rho2) -> let
            var = newVar (vars e1 ++ vars e2 ++ Map.keys env1 ++ Map.keys env2) s1
            e1' = eval (extend rho1 s1 (Val (Var var (Just t1)))) e1
            e2' = eval (extend rho2 s2 (Val (Var var (Just t2)))) e2
            in conv' info dirs t1 t2 &&
               conv' (i + 1, Map.insert var i env1, Map.insert var i env2) dirs e1' e2'
        (Abst s1 t1 e1,Abst s2 t2 e2) -> conv' info dirs t1 t2 &&
            conv' (i + 1, Map.insert s1 i env1, Map.insert s2 i env2) dirs e1 e2
        (Universe,Universe) -> True
        {- Naturals -}
        (Nat,Nat)           -> True
        (Zero,Zero)         -> True
        (Succ n1,Succ n2)   -> conv' info dirs n1 n2
        {- Cubical -}
        (I,I)               -> True
        (I0,I0)             -> True
        (I1,I1)             -> True
        (Sys sys1,Sys sys2) -> conv' info dirs sys1 sys2
        (Sys sys1,v2)       -> conv' info dirs (simplifySystem dirs sys1) v2
        (v1,Sys sys2)       -> conv' info dirs v1 (simplifySystem dirs sys2)
        (Partial phi1 v1,Partial phi2 v2) -> conv' info dirs phi1 phi2 && conv' info dirs v1 v2
        {- Neutrals -}
        (Var s1 _,Var s2 _) -> case (Map.lookup s1 env1, Map.lookup s2 env2) of
            (Nothing,Nothing) -> s1 == s2 --free variables
            (Just i,Just j)   -> i == j   --bound variables
            otherwise         -> False    --one free, one bound
        (App fun1 arg1,App fun2 arg2) -> conv' info dirs fun1 fun2 && conv' info dirs arg1 arg2
        (Ind ty1 b1 s1 n1,Ind ty2 b2 s2 n2) ->
            conv' info dirs ty1 ty2 && conv' info dirs b1  b2 &&
            conv' info dirs s1  s2  && conv' info dirs n1  n2
        otherwise             -> False

instance Convertible System where
    conv' info@(i,env1,env2) dirs sys1 sys2 = 
        conv' info dirs (foldOr $ keys sys1) (foldOr $ keys sys2) &&
        all (\(conjs,t1,t2) -> trace ("conjs = " ++ show conjs ++ ", t1 = " ++ show t1 ++ ", t2 = " ++ show t2) $
                all (\conj -> conv' info (addConj conj dirs) t1 t2) conjs) meets
        where meets = [(toDNFList phi, sys1 `at` phi1, sys2 `at` phi2) | phi1 <- keys sys1, phi2 <- keys sys2,
                (phi1 `imp` phi2 || phi2 `imp` phi1), phi <- if phi1 `imp` phi2 then [phi1] else [phi2]]


