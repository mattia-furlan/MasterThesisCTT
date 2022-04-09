{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

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

inferType :: Ctx -> Term -> Either ErrorString Value
inferType ctx t = case t of
    Var s -> return $ eval (ctxToEnv ctx) (fromRight Universe $ lookupType ctx s) --Universe is silly default TODO
    Universe -> Right Universe
    App fun arg -> do
        funTy <- inferType ctx fun
        case funTy of
            c@(Closure s tVal e _) -> do
                checkType ctx arg tVal
                let argVal = eval (ctxToEnv ctx) arg
                return $ doApply c argVal
            otherwise -> Left $
                "term '" ++ show fun ++ "' has type '" ++ show funTy ++ "' , which is not a product"
    Nat -> Right Universe
    Zero -> Right Nat
    Succ n -> do
        checkType ctx n Nat
        Right Nat 
    Ind ty base step n -> do
        nTyVal <- inferType ctx n
        isNat n nTyVal
        checkType ctx ty (eval emptyEnv (Abst (Ident "_") Nat Universe))
        let tyVal  = eval (ctxToEnv ctx) ty
            tyVal0 = doApply tyVal Zero
        checkType ctx base tyVal0
        checkType ctx step (eval (ctxToEnv ctx)
            (Abst (Ident "n") Nat (Abst (Ident "_") (App ty (Var (Ident "n")))
                (App ty (Succ (Var (Ident "n")))))))
        let nVal = eval (ctxToEnv ctx) n
        return $ doApply tyVal nVal
    I -> Right Universe --TODO ?
    _ -> Left $ "don't know how to infer type of '" ++ show t ++ "'"


isNat :: Term -> Value -> Either ErrorString ()
isNat t Nat = Right ()
isNat t v = Left $ "expected type nat, got term '" ++ show t ++
                "' of type '" ++ show v ++ "' instead"


checkType :: Ctx -> Term -> Value -> Either ErrorString ()
checkType ctx e v = case ctx of
    ((_,Form phi) : ctx') -> do
        let conjs = toDNFList phi
        return ()
        --TODO
        --mapM_ (\conj -> checkType ctx' (multipleSubst e conj) (multipleSubst v conj)) conjs
    otherwise -> case (e,v) of
        (Abst s t e,Universe) -> do
            checkType ctx t Universe
            checkType (extend ctx s (Decl t)) e Universe
        (Abst s t e,Closure s1 t1Val e1 rho) -> do
            checkType ctx t Universe
            let tVal = eval (ctxToEnv ctx) t
            unless (conv tVal t1Val) $
                Left $ "type '" ++ show tVal ++ "' is not convertible to type '" ++ show t1Val ++
                    "' (while checking term '" ++ show (Abst s t e) ++ "' against type '" ++ show v ++ "')"
            --Instead of 's' I should choose a new name ('s' could be already in the context)
            let var = newVar (keys ctx ++ [s,s1]) s
            let e1Val = eval (extend rho s1 (Val (Var var))) e1
            checkType (extend ctx s (Def t (Var var))) e e1Val
        (Sys sys,Partial phi ty) -> do
            let phis = keys sys
            mapM_ (checkFormula ctx) phis
            unless (equalFormulas (foldOr phis) phi) $ --TODO
                Left $ "'" ++ show (foldOr phis) ++ "' is not equivalent to '" ++ show phi ++ "'"
            mapM_ (\(phi',t) -> checkType (extend ctx (Ident "_") (Form phi')) t ty) sys
        (Partial phi ty,Universe) -> do
            let ctx' = extend ctx (Ident "_") (Form phi)
            checkType ctx' ty Universe
        otherwise -> do
            ty <- inferType ctx e
            unless (conv v ty) $
                Left $ "type '" ++ show v ++ "' expected, got term '" ++ show e
                       ++ "' of type '" ++ show ty ++ "' instead"

checkFormula :: Ctx -> Formula -> Either ErrorString ()
checkFormula ctx ff = do
    let dom     = keys ctx
        support = vars ff
    unless (all (`elem` dom) support) $
        Left $ "formula '" ++ show ff ++ "' contains undeclared names"

class Convertible a where
    conv :: a -> a -> Bool
    conv = conv' (0, Map.empty, Map.empty)
    conv' :: (Int, Map Ident Int, Map Ident Int) -> a -> a -> Bool

instance Convertible Formula where
    conv' _ ff1 ff2 = equalFormulas ff1 ff2

instance Convertible Term where
    conv' info@(i,env1,env2) v1 v2 = v1 == v2 || case (v1,v2) of
        (Closure s1 t1 e1 rho1,Closure s2 t2 e2 rho2) -> let
            var = newVar (vars e1 ++ vars e2 ++ Map.keys env1 ++ Map.keys env2) s1
            e1' = eval (extend rho1 s1 (Val (Var var))) e1
            e2' = eval (extend rho2 s2 (Val (Var var))) e2
            in conv' info t1 t2 &&
               conv' (i + 1, Map.insert var i env1, Map.insert var i env2) e1' e2'
        (Abst s1 t1 e1,Abst s2 t2 e2) -> conv' info t1 t2 &&
            conv' (i + 1, Map.insert s1 i env1, Map.insert s2 i env2) e1 e2
        (Universe,Universe) -> True
        {- Naturals -}
        (Nat,Nat)           -> True
        (Zero,Zero)         -> True
        (Succ n1,Succ n2)   -> conv' info n1 n2
        {- Cubical -}
        (I,I)               -> True
        (Sys sys1,Sys sys2) -> conv' info sys1 sys2
        (Partial phi1 v1,Partial phi2 v2) -> conv' info phi1 phi2 && conv' info v1 v2
        {- Neutrals -}
        (Var s1,Var s2) -> case (Map.lookup s1 env1, Map.lookup s2 env2) of
            (Nothing,Nothing) -> s1 == s2 --free variables
            (Just i,Just j)   -> i == j   --bound variables
            otherwise         -> False    --one free, one bound
        (App fun1 arg1,App fun2 arg2) -> conv' info fun1 fun2 && conv' info arg1 arg2
        (Ind ty1 b1 s1 n1,Ind ty2 b2 s2 n2) ->
            conv' info ty1 ty2 && conv' info b1  b2 && conv' info s1  s2 && conv' info n1  n2
        otherwise             -> False
 
--convPartial :: (Convertible a, Sub a) => Formula -> a -> a -> Bool

instance Convertible System where
    conv' info@(i,env1,env2) sys1 sys2 =
        conv' info (foldOr $ keys sys1) (foldOr $ keys sys2) {-&&
        all (\(conjs,t1,t2) -> --TODO!! 
                all (\conj -> conv' info (multipleSubst t1 conj) (multipleSubst t2 conj)) conjs) terms-}
        where meets = [(phi1,phi2) | phi1 <- keys sys1, phi2 <- keys sys2]
              ints = filter (not . \(phi1,phi2) -> equalFormulas (makeAnd phi1 phi2) FFalse) meets
              terms = [(toDNFList (phi1 `makeAnd` phi2), sys1 `at` phi1, sys2 `at` phi2) | (phi1,phi2) <- ints]


