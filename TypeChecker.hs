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
    Var s _ -> Right $ eval ctx dirs (ctxToEnv ctx) (lookupType ctx s)
        where forceRight eith = case eith of Right r -> r
    Universe -> Right Universe
    App fun arg -> do
        funTy <- inferType ctx dirs fun
        case funTy of
            c@(Closure s tVal e _) -> do
                checkType ctx dirs arg tVal
                let argVal = eval ctx dirs (ctxToEnv ctx) arg
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
        checkType ctx dirs ty (eval emptyCtx emptyDirEnv emptyEnv (Abst (Ident "_") Nat Universe))
        let tyVal  = eval ctx dirs (ctxToEnv ctx) ty
            tyVal0 = doApply tyVal Zero
        checkType ctx dirs base tyVal0
        checkType ctx dirs step (eval ctx dirs (ctxToEnv ctx)
            (Abst (Ident "n") Nat (Abst (Ident "_") (App ty (Var (Ident "n") (Just Nat)))
                (App ty (Succ (Var (Ident "n") (Just Nat)))))))
        let nVal = eval ctx dirs (ctxToEnv ctx) n
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
checkType ctx dirs e v = let eOrig = e in case (e,v) of
    (Abst s t e,Universe) -> do
        checkType ctx dirs t Universe
        checkType (extend ctx s (Decl t)) dirs e Universe
    (Abst s t e,Closure s1 t1Val e1 (ctx1,dirs1,rho)) -> myTrace (" >> e = " ++ show eOrig ++ ", v = " ++ show v ++ ", ctx = " ++ {-showCtx (ctx)-} "..." ++ ", dirs = " ++ show dirs) $  {-myTrace ("s = " ++ show s ++ ", t = " ++ show t ++ ", e = " ++ show e ++ ", s1 = " ++ show s1 ++ ", t1 = " ++ show t1Val ++ ", e1 = " ++ show e1) $-} do
        checkType ctx dirs t Universe
        let tVal = eval ctx dirs (ctxToEnv ctx) t
        unless (conv tVal t1Val) $
            Left $ "type '" ++ show tVal ++ "' is not convertible to type '" ++ show t1Val ++
                "' (while checking term '" ++ show (Abst s t e) ++ "' against type '" ++ show v ++ "')"
        
        if tVal == I then do --TODO unire `dirs` e `dirs1`!
            let e1Val = eval (extend ctx1 s1 (Decl I)) (dirs1 +++ dirs) (rho ++ ctxToEnv ctx) e1
            when (conv e1Val I) $
                Left $ "I cannot appear as codomain in products"
            checkType (extend ctx s (Decl I)) dirs e e1Val
        else do
            --Instead of 's' I should choose a new name ('s' could be already in the context)
            let var = newVar (keys ctx ++ [s,s1]) s
            let e1Val = eval (extend ctx1 var (Decl t)) (dirs1 +++ dirs) (extend rho s1 (Val (Var var (Just t1Val)))) e1
            when (conv e1Val I) $
                Left $ "I cannot appear as codomain in products"
            checkType (extend (extend ctx var (Decl t)) s (Def t (Var var (Just tVal)))) dirs e e1Val
    (Sys sys,{-Partial phi ty-}v) -> myTrace (">> sys = " ++ show sys ++ ", v = " ++ show v) $ do
        let (phi,ty) = case v of
                Partial phi ty -> (phi,ty)
                otherwise      -> (FTrue,v)  --TODO is this necessary?
        let phis = map (evalFormula dirs) (keys sys)
        mapM (checkFormula ctx) phis
        unless (conv (evalFormula dirs $ foldOr phis) (evalFormula dirs phi)) $
            Left $ "formulas don't match"
        mapM (\(phi',t) -> checkTypePartial phi' ctx dirs t ty) sys
        let eq_check = all (\((phi1,t1),(phi2,t2)) -> convPartial (phi1 `makeAnd` phi2) (eval ctx dirs (ctxToEnv ctx) t1) (eval ctx dirs (ctxToEnv ctx) t2))
                        [(x1,x2) | x1 <- sys, x2 <- sys, x1 /= x2]
        unless eq_check $
            Left $ "values are not adjacent"
    (Partial phi ty,Universe) -> do
        when (conv ty I) $
            Left $ "I is not a type"
        checkTypePartial phi ctx dirs ty Universe
    (Restr phi u ty,Universe) -> do
        when (conv ty I) $
            Left $ "I is not a type"
        checkType ctx dirs ty Universe
        let tyVal = eval ctx dirs (ctxToEnv ctx) ty
        checkTypePartial phi ctx dirs u tyVal
    (e,Restr phi u ty) -> do
        let eVal = eval ctx dirs (ctxToEnv ctx) e
        checkType ctx dirs e ty
        unless (convPartial phi eVal u) $
            Left $ "term '" ++ show e ++ "' does not agree with '" ++ show u ++ "' on " ++ show phi
    otherwise -> do
        ty <- inferType ctx dirs e
        let ty' = case ty of
                Restr _ _ v' -> v'
                otherwise    -> ty
        unless (conv v ty') $
            Left $ "type '" ++ show v ++ "' expected, got term '" ++ show e
                   ++ "' of type '" ++ show ty ++ "' instead"


checkFormula :: Ctx -> Formula -> Either ErrorString ()
checkFormula ctx ff = do
    let dom     = keys ctx
        support = vars ff
    unless (all (`elem` dom) support) $
        Left $ "formula '" ++ show ff ++ "' contains undeclared names"

class Convertible a where
    conv' :: (Int, Map Ident Int, Map Ident Int) -> a -> a -> Bool

    conv :: a -> a -> Bool
    conv = conv' (0, Map.empty, Map.empty)

{-
convPartial :: Ctx -> DirEnv -> Formula -> Term -> Term -> Bool
convPartial ctx dirs ff t1 t2 = all (\conj -> myTrace ("[convPartial] conj = " ++ show conj ++ ", t1 = " ++ show t1 ++ ", t2 = " ++ show t2 ++ ", t1Val = " ++ show (eval ctx (toDirEnv conj) (ctxToEnv ctx) t1) ++ ", t2Val = " ++ show (eval ctx (toDirEnv conj) (ctxToEnv ctx) t2)) $
    conv dirs (eval ctx (toDirEnv conj) (ctxToEnv ctx) t1) (eval ctx (toDirEnv conj) (ctxToEnv ctx) t2)) conjs
    where conjs = toDNFList ff
-}

convPartial :: Formula -> Value -> Value -> Bool
convPartial ff t1 t2 = all (\conj -> myTrace ("[convPartial] conj = " ++ show conj ++ ", t1 = " ++ show t1 ++ ", t2 = " ++ show t2 ++ ", t1vs = " ++  show (simplifyValue (toDirEnv conj) t1) ++ ", t2vs = " ++ show (simplifyValue (toDirEnv conj) t2)) $
    conv (simplifyValue (toDirEnv conj) t1) (simplifyValue (toDirEnv conj) t2)) conjs
    where conjs = toDNFList ff


instance Convertible Formula where
    conv' _ ff1 ff2 = equalFormulas ff1 ff2

instance Convertible Term where
    conv' info@(i,env1,env2) v1 v2 = myTrace ("[conv] v1 = " ++ show v1 ++ ", v2 = " ++ show v2) $  v1 == v2 || case (v1,v2) of
        (Closure s1 t1 e1 (ctx1,dirs1,rho1),Closure s2 t2 e2 (ctx2,dirs2,rho2)) -> let
            var = newVar (vars e1 ++ vars e2 ++ Map.keys env1 ++ Map.keys env2) s1
            e1' = eval ctx1 dirs1 (extend rho1 s1 (Val (Var var (Just t1)))) e1 --TODO unire
            e2' = eval ctx2 dirs2 (extend rho2 s2 (Val (Var var (Just t2)))) e2 --TODO unire
            in conv' info t1 t2 &&
               conv' (i + 1, Map.insert var i env1, Map.insert var i env2) e1' e2'
        -- (Abst s1 t1 e1,Abst s2 t2 e2) -> conv' info dirs t1 t2 &&
        --     conv' (i + 1, Map.insert s1 i env1, Map.insert s2 i env2) dirs e1 e2
        (Universe,Universe) -> True
        {- Naturals -}
        (Nat,Nat)           -> True
        (Zero,Zero)         -> True
        (Succ n1,Succ n2)   -> conv' info n1 n2
        {- Cubical -}
        (I,I)               -> True
        (I0,I0)             -> True
        (I1,I1)             -> True
        (Sys sys1,Sys sys2) -> conv' info sys1 sys2
        -- (Sys sys1,v2)       -> conv' info dirs (simplifyValue dirs (Sys sys1)) v2
        -- (v1,Sys sys2)       -> myTrace ("sys2' = " ++ show (simplifyValue dirs (Sys sys2))) $ conv' info dirs v1 (simplifyValue dirs (Sys sys2))
        (Partial phi1 v1,Partial phi2 v2) -> conv' info phi1 phi2 && conv' info v1 v2
        {- Neutrals -}
        (Var s1 _,Var s2 _) -> case (Map.lookup s1 env1, Map.lookup s2 env2) of
            (Nothing,Nothing) -> s1 == s2 --free variables
            (Just i,Just j)   -> i == j   --bound variables
            otherwise         -> False    --one free, one bound
        -- (App fun1 arg1,v2) | isIAbst fun1 -> myTrace ("App,v2") $ conv' info dirs (fun1 @@ arg1) v2
        -- (v1,App fun2 arg2) | isIAbst fun2 -> myTrace ("v1,App") $ conv' info dirs v1 (fun2 @@ arg2)
        (App fun1 arg1,App fun2 arg2) -> conv' info fun1 fun2 && conv' info arg1 arg2
        (Ind ty1 b1 s1 n1,Ind ty2 b2 s2 n2) ->
            conv' info ty1 ty2 && conv' info b1  b2 &&
            conv' info s1  s2  && conv' info n1  n2
        otherwise             -> False

instance Convertible System where
    conv' info@(i,env1,env2) sys1 sys2 = 
        conv' info (foldOr $ keys sys1) (foldOr $ keys sys2) &&
        all (\(conjs,t1,t2) -> myTrace ("conjs = " ++ show conjs ++ ", t1 = " ++ show t1 ++ ", t2 = " ++ show t2) $
                all (\conj -> conv' info (simplifyValue (toDirEnv conj) t1) (simplifyValue (toDirEnv conj) t2)) conjs) meets
        where meets = [(toDNFList phi, sys1 `at` phi1, sys2 `at` phi2) | phi1 <- keys sys1, phi2 <- keys sys2,
                (phi1 `imp` phi2 || phi2 `imp` phi1), phi <- if phi1 `imp` phi2 then [phi1] else [phi2]]


