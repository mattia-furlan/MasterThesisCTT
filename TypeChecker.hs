{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module TypeChecker where

import Data.Either
import qualified Data.Map as Map
import Control.Monad

import Ident
import Interval
import CoreCTT
import Eval

import Debug.Trace

inferType :: Ctx -> Term -> Either ErrorString Value
inferType ctx t = case t of
    Var s -> return $ eval (ctxToEnv ctx) (forceRight $ lookupType ctx s)
    Universe -> Right VUniverse
    App fun arg -> do
        funTy <- inferType ctx fun
        case funTy of
            c@(VClosure s tVal e _) -> do
                checkType ctx arg tVal
                let argVal = eval (ctxToEnv ctx) arg
                return $ doApply c argVal
            otherwise -> Left $
                "term '" ++ showTerm fun AsTerm ++ "' has type '" ++ showValue funTy AsType ++ "' , which is not a product"
    Nat -> Right VUniverse
    Zero -> Right VNat
    Succ n -> do
        checkType ctx n VNat
        Right VNat 
    Ind ty base step n -> do
        nTyVal <- inferType ctx n
        isNat n nTyVal
        checkType ctx ty (eval emptyEnv (Abst (Ident "_") Nat Universe))
        let tyVal  = eval (ctxToEnv ctx) ty
            tyVal0 = doApply tyVal VZero
        checkType ctx base tyVal0
        checkType ctx step (eval (ctxToEnv ctx)
            (Abst (Ident "n") Nat (Abst (Ident "_") (App ty (Var (Ident "n")))
                (App ty (Succ (Var (Ident "n")))))))
        let nVal = eval (ctxToEnv ctx) n
        return $ doApply tyVal nVal
    I -> Right VUniverse --TODO ?
    _ -> Left $ "don't know how to infer type of '" ++ showTerm t AsTerm ++ "'"


isNat :: Term -> Value -> Either ErrorString ()
isNat t VNat = Right ()
isNat t v = Left $ "expected type nat, got term '" ++ showTerm t AsType ++
                "' of type '" ++ showValue v AsType ++ "' instead"


checkType :: Ctx -> Term -> Value -> Either ErrorString ()
checkType ctx e v = case ctx of
    Env ((_,Form phi) : ctx') -> do
        let conjs = toDNFList phi
        mapM_ (\conj -> checkType (Env ctx') (multipleSubst e conj) (multipleSubst v conj)) conjs
    otherwise -> case (e,v) of
        (Abst s t e,VUniverse) -> do
            checkType ctx t VUniverse
            checkType (extendEnv ctx s (Decl t)) e VUniverse
        (Abst s t e,VClosure s1 t1Val e1 rho) -> trace ("e = " ++ show e ++ ", t = " ++ show t) $ do
            checkType ctx t VUniverse
            let tVal = eval (ctxToEnv ctx) t
            unless (conv tVal t1Val) $
                Left $ "type '" ++ showValue tVal AsType ++ "' is not convertible to type '" ++ showValue t1Val AsType ++
                    "' (while checking term '" ++ showTerm (Abst s t e) AsTerm ++ "' against type '" ++ showValue v AsType ++ "')"
            --Instead of 's' I should choose a new name ('s' could be already in the context)
            let var = newVar (getIdentsEnv ctx ++ [s,s1]) s
            let e1Val = eval (extendEnv rho s1 (Val (VVar var))) e1
            checkType (extendEnv ctx s (Def t (Var var))) e e1Val
        (Sys sys,VPartial phi ty) -> do
            let phis = Map.keys sys
            mapM_ (checkFormula ctx) phis
            unless (equalFormulas (foldOr phis) phi) $ --TODO
                Left $ "'" ++ show (foldOr phis) ++ "' is not equivalent to '" ++ show phi ++ "'"
            mapM_ (\(phi',t) -> checkType (extendEnv ctx (Ident "_") (Form phi')) t ty) (Map.toList sys)
        (Partial phi ty,VUniverse) -> do
            let ctx' = extendEnv ctx (Ident "_") (Form phi)
            checkType ctx' ty VUniverse
        otherwise -> do
            ty <- inferType ctx e
            let ok = conv v ty
            unless ok $
                Left $ "type '" ++ showValue v AsType ++ "' expected, got term '" ++ showTerm e AsTerm
                       ++ "' of type '" ++ showValue ty AsType ++ "' instead"

checkFormula :: Ctx -> Formula -> Either ErrorString ()
checkFormula ctx ff = do
    let dom     = getIdentsEnv ctx
        support = vars ff
    unless (all (`elem` dom) support) $
        Left $ "formula '" ++ show ff ++ "' contains undeclared names"


class Convertible a where
    conv :: a -> a -> Bool
    conv = conv' (0,emptyEnv,emptyEnv)
    conv' :: (Int,Env Int, Env Int) -> a -> a -> Bool

instance Convertible Formula where
    conv' _ ff1 ff2 = equalFormulas ff1 ff2

instance Convertible Value where
    conv' info@(i,env1,env2) v1 v2 = case (v1,v2) of
        (VClosure s1 t1 e1 rho1,VClosure s2 t2 e2 rho2) -> let
            e1' = eval rho1 e1
            e2' = eval rho2 e2
            in conv' (i+1,extendEnv env1 s1 i,extendEnv env2 s2 i) e1' e2'
        (VUniverse,VUniverse) -> True
        {- Naturals -}
        (VNat,VNat)           -> True
        (VZero,VZero)         -> True
        (VSucc n1,VSucc n2)   -> conv' info n1 n2
        {- Cubical -}
        (VI,VI)               -> True
        (VSys sys1,VSys sys2) -> True
        (VPartial phi1 v1,VPartial phi2 v2) -> conv' info phi1 phi2 && conv' info v1 v2
        {- Neutrals -}
        (VVar s1,VVar s2) -> case (lookupEnv env1 s1,lookupEnv env2 s2) of
            (Left _,Left _)   -> s1 == s2 --free variables
            (Right i,Right j) -> i == j   --bound variables
            otherwise         -> False    --one free, one bound
        (VApp fun1 arg1,VApp fun2 arg2) -> conv' info fun1 fun2 && conv' info fun2 arg2
        (VInd ty1 b1 s1 n1,VInd ty2 b2 s2 n2) ->
            conv' info ty1 ty2 && conv' info b1  b2 && conv' info s1  s2 && conv' info n1  n2
        otherwise             -> False
 
--convPartial :: (Convertible a, Sub a) => Formula -> a -> a -> Bool

instance (Convertible a, Sub a) => Convertible (System a) where
    conv' info@(i,env1,env2) sys1 sys2 =
        conv' info (foldOr $ Map.keys sys1) (foldOr $ Map.keys sys2) &&
        all (\(conjs,t1,t2) ->
                all (\conj -> conv' info (multipleSubst t1 conj) (multipleSubst t2 conj)) conjs)
            terms
        where meets = [(phi1,phi2) | phi1 <- Map.keys sys1, phi2 <- Map.keys sys2]
              ints = filter (not . \(phi1,phi2) -> equalFormulas (makeAnd phi1 phi2) FFalse) meets
              terms = [(toDNFList (phi1 `makeAnd` phi2), sys1 Map.! phi1, sys2 Map.! phi2) | (phi1,phi2) <- ints]


