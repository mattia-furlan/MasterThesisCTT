module TypeChecker where

import Data.Either
import qualified Data.Map as Map
import Control.Monad

import CoreCTT
import Eval


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
    _ -> Left $ "don't know how to infer type of '" ++ showTerm t AsTerm ++ "'"


isNat :: Term -> Value -> Either ErrorString ()
isNat t VNat = Right ()
isNat t v = Left $ "expected type nat, got term '" ++ showTerm t AsType ++
                "' of type '" ++ showValue v AsType ++ "' instead"


checkType :: Ctx -> Term -> Value -> Either ErrorString ()
checkType ctx e v = case (e,v) of
    (Abst s t e,VUniverse) -> do
        checkType ctx t VUniverse
        checkType (extendEnv ctx s (Decl t)) e VUniverse
    (Abst s t e,VClosure s1 t1Val e1 rho) -> do
        checkType ctx t VUniverse
        let tVal = eval (ctxToEnv ctx) t
        unless (alphaEquivValue ctx tVal t1Val) $
            Left $ "type '" ++ showValue tVal AsType ++ "' is not convertible to type '" ++ showValue t1Val AsType ++
                "' (while checking term '" ++ showTerm (Abst s t e) AsTerm ++ "' against type '" ++ showValue v AsType ++ "')"
        let e1Val = eval (extendEnv rho s1 (Val (VVar s))) e1
        checkType (extendEnv ctx s (Decl t)) e e1Val
    (Nat,VUniverse) -> Right ()
    (Zero,VNat) -> Right ()
    (Succ n,VNat) -> checkType ctx n VNat
    otherwise -> do
        ty <- inferType ctx e
        let ok = alphaEquivValue ctx v ty
        unless ok $
            Left $ "type '" ++ showValue v AsType ++ "' expected, got term '" ++ showTerm e AsTerm
                   ++ "' of type '" ++ showValue ty AsType ++ "' instead"


alphaEquiv :: Term -> Term -> Bool
alphaEquiv e1 e2 = alphaEquivHelper 0 emptyEnv e1 emptyEnv e2
    where
        alphaEquivHelper :: Int -> Env Int -> Term -> Env Int -> Term -> Bool
        alphaEquivHelper i env1 (Var s1) env2 (Var s2) = case (lookupEnv env1 s1,lookupEnv env2 s2) of
            (Left _,Left _)   -> s1 == s2 --free variables
            (Right i,Right j) -> i == j   --bound variables
            otherwise         -> False    --one free, one bound
        alphaEquivHelper _ _ Universe _ Universe = True --i1 == i2
        alphaEquivHelper i env1 (Abst s1 t1 e1) env2 (Abst s2 t2 e2) = 
            alphaEquivHelper i env1 t1 env2 t2 && --same types
            alphaEquivHelper (i + 1) (extendEnv env1 s1 i) e1 (extendEnv env2 s2 i) e2
        alphaEquivHelper i env1 (App fun1 arg1) env2 (App fun2 arg2) = 
            alphaEquivHelper i env1 fun1 env2 fun2 &&
            alphaEquivHelper i env1 arg1 env2 arg2
        alphaEquivHelper _ _ Nat _ Nat = True
        alphaEquivHelper _ _ Zero _ Zero = True
        alphaEquivHelper i env1 (Succ e1) env2 (Succ e2) = alphaEquivHelper i env1 e1 env2 e2 
        alphaEquivHelper i env1 (Ind ty1 b1 s1 n1) env2 (Ind ty2 b2 s2 n2) = 
            alphaEquivHelper i env1 ty1 env2 ty2 &&
            alphaEquivHelper i env1 b1 env2 b2 &&
            alphaEquivHelper i env1 s1 env2 s2 &&
            alphaEquivHelper i env1 n1 env2 n2  
        alphaEquivHelper _ _ _ _ _ = False

--Probably too inefficient (?)
--Or maybe I should implement only `alphaEquivValue` instead of `alphaEquiv`
alphaEquivValue :: Ctx -> Value -> Value -> Bool
alphaEquivValue ctx v1 v2 = alphaEquiv (readBack [] v1) (readBack [] v2)
        {-alphaEquivValueHelper 0 emptyEnv v1 emptyEnv v2
    where
        alphaEquivValueHelper :: Int -> Env Int -> Value -> Env Int -> Value -> Either ErrorString Bool
        alphaEquivValueHelper i env1 (VVar s1) env2 (VVar s2) = case (lookupEnv env1 s1,lookupEnv env2 s2) of
            (Left _,Left _)   -> Right $ s1 == s2 --free variables
            (Right i,Right j) -> Right $ i == j   --bound variables
            otherwise         -> Right False    --one free, one bound
        alphaEquivValueHelper _ _ (VUniverse) _ (VUniverse) = Right True --i1 == i2
        alphaEquivValueHelper i env1 (VClosure (Abst s1 t1 e1) rho1) env2 (VClosure (Abst s2 t2 e2) rho2) = do
            t1' <- eval rho1 t1
            t2' <- eval rho2 t2
            alphaEquivValueHelper i env1 t1' env2 t2'

        alphaEquivValueHelper i env1 (VApp fun1 arg1) env2 (VApp fun2 arg2) = do
            b1 <- alphaEquivValueHelper i env1 fun1 env2 fun2
            b2 <- alphaEquivValueHelper i env1 arg1 env2 arg2
            return $ b1 && b2
        alphaEquivValueHelper _ _ VNat _ VNat = Right True
        alphaEquivValueHelper _ _ VZero _ VZero = Right True
        alphaEquivValueHelper i env1 (VSucc e1) env2 (VSucc e2) = alphaEquivValueHelper i env1 e1 env2 e2 
        alphaEquivValueHelper _ _ _ _ _ = Right False-}
