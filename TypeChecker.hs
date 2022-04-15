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
    App fun arg -> do
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
    Ind ty base step n -> do
        nTyVal <- inferType ctx dirs n
        isNat n nTyVal
        checkType ctx dirs ty (eval emptyCtx emptyDirEnv (Abst (Ident "_") Nat Universe))
        let tyVal  = eval ctx dirs ty
            tyVal0 = doApply tyVal Zero
        checkType ctx dirs base tyVal0
        checkType ctx dirs step (eval ctx dirs 
            (Abst (Ident "n") Nat (Abst (Ident "_") (App ty (Var (Ident "n") (Just Nat)))
                (App ty (Succ (Var (Ident "n") (Just Nat)))))))
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
    mapM_ (\conj -> checkType ctx (addConj conj dirs) e v) conjs

checkType :: Ctx -> DirEnv -> Term -> Value -> Either ErrorString ()
checkType ctx dirs e v = myTrace ("[checkType]>> e = " ++ show e ++ ", v = " ++ show v ++ ", ctx = " ++ {-showCtx (ctx)-} "..." ++ ", dirs = " ++ show dirs) $ case (e,v) of
    (Abst s t e,Universe) -> do
        checkType ctx dirs t Universe
        checkType (extend ctx s (Decl t)) dirs e Universe
    (Abst s t e,Closure s1 t1Val e1 (ctx1,dirs1)) -> {-myTrace ("s = " ++ show s ++ ", t = " ++ show t ++ ", e = " ++ show e ++ ", s1 = " ++ show s1 ++ ", t1 = " ++ show t1Val ++ ", e1 = " ++ show e1) $-} do
        checkType ctx dirs t Universe
        let tVal = eval ctx dirs t
        {-unless (conv [] tVal t1Val) $
            Left $ "type '" ++ show tVal ++ "' is not convertible to type '" ++ show t1Val ++
                "' (while checking term '" ++ show (Abst s t e) ++ "' against type '" ++ show v ++ "')"-}
        
        if tVal == I then do
            let e1Val = eval (extend ctx1 s1 (Decl I) {- ++ ctx ?? -}) dirs1 e1
            --when (conv [] e1Val I) $
            when (e1Val == I) $
                Left $ "I cannot appear as codomain in products"
            checkType (extend ctx s (Decl I)) (addDiag dirs s s1) e e1Val
        else do
            --Instead of 's' I should choose a new name ('s' could be already in the context)
            let var   = newVar (keys ctx ++ keys ctx1) s
                ctx1' = if s1 == Ident "" then
                        ctx1
                    else if s1 == var then
                        extend ctx1 s1 (VDecl t1Val)
                    else
                        --extend (extend ctx1 var (Decl t)) s1 (Val (Var var (Just t1Val)))
                        extend ctx1 s1 (Val (Var var (Just t1Val)))
                e1Val = eval ctx1' dirs1 e1
            --when (conv [] e1Val I) $
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
        unless (conv [] (evalFormula dirs $ foldOr phis) (evalFormula dirs phi)) $
            Left $ "formulas don't match"
        mapM (\(phi',t) -> checkTypePartial phi' ctx dirs t ty) sys
        let eq_check = all (\((phi1,t1),(phi2,t2)) -> convPartial (phi1 `makeAnd` phi2) (eval ctx dirs t1) (eval ctx dirs t2))
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
            phi = (getSystemFormula sys)
        checkTypePartial phi ctx dirs (Sys sys) tyVal
    (e,Restr sys ty) -> do
        let eVal = eval ctx dirs e
            phi = (getSystemFormula sys)
        checkType ctx dirs e ty
        unless (convPartial phi eVal (Sys sys)) $
            Left $ "term '" ++ show e ++ "' does not agree with '" ++ show sys ++ "' on " ++ show phi
    otherwise -> do
        ty <- inferType ctx dirs e
        let ty' = case ty of
                Restr _ v' -> v'
                otherwise  -> ty
        unless (conv [] v ty') $
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

convPartial :: Formula -> Value -> Value -> Bool
convPartial ff t1 t2 = all (\conj -> myTrace ("[convPartial] conj = " ++ show conj ++ ", t1 = " ++ show t1 ++ ", t2 = " ++ show t2 ++ ", t1vs = " ++  show (simplifyValue (toDirEnv conj) t1) ++ ", t2vs = " ++ show (simplifyValue (toDirEnv conj) t2)) $
    conv [] (simplifyValue (toDirEnv conj) t1) (simplifyValue (toDirEnv conj) t2)) conjs
    where conjs = toDNFList ff

instance Convertible Formula where
    conv _ ff1 ff2 = equalFormulas ff1 ff2

instance Convertible Value where
    conv used v1 v2 = myTrace ("[conv] v1 = " ++ show v1 ++ ", v2 = " ++ show v2) $  v1 == v2 || case (v1,v2) of
        (Closure s1 t1 e1 (ctx1,dirs1),Closure s2 t2 e2 (ctx2,dirs2)) -> let
            var = newVar used s1
            e1' = eval (extend ctx1 s1 (Val (Var var (Just t1)))) dirs1 e1
            e2' = eval (extend ctx2 s2 (Val (Var var (Just t2)))) dirs2 e2
            in conv (var : used) t1 t2 && conv (var : used) e1' e2'
        -- (Abst s1 t1 e1,Abst s2 t2 e2) -> conv used dirs t1 t2 &&
        --     conv used (i + 1, Map.insert s1 i env1, Map.insert s2 i env2) dirs e1 e2
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
        {- Neutrals -}
        (Var s1 _,Var s2 _) -> s1 == s2
        (App fun1 arg1,App fun2 arg2) -> conv used fun1 fun2 && conv used arg1 arg2
        (Ind ty1 b1 s1 n1,Ind ty2 b2 s2 n2) ->
            conv used ty1 ty2 && conv used b1  b2 &&
            conv used s1  s2  && conv used n1  n2
        otherwise             -> False

instance Convertible System where
    conv used sys1 sys2 = 
        conv used (foldOr $ keys sys1) (foldOr $ keys sys2) &&
        all (\(conjs,t1,t2) -> myTrace ("conjs = " ++ show conjs ++ ", t1 = " ++ show t1 ++ ", t2 = " ++ show t2) $
                all (\conj -> conv used (simplifyValue (toDirEnv conj) t1) (simplifyValue (toDirEnv conj) t2)) conjs) meets
        where meets = [(toDNFList phi, sys1 `at` phi1, sys2 `at` phi2) | phi1 <- keys sys1, phi2 <- keys sys2,
                (phi1 `imp` phi2 || phi2 `imp` phi1), phi <- if phi1 `imp` phi2 then [phi1] else [phi2]]


