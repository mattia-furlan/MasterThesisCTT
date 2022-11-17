module TypeChecker where

import Control.Monad

import Ident
import Interval
import CoreCTT
import Eval
import Conv

-- Infer the type of a term, in the given context and directions environment
inferType :: Ctx -> DirEnv -> Term -> Either ErrorString Value
inferType ctx dirs term = myTrace ("[inferType] " ++ show term ++ ", ctx = " ++ showCtx ctx ++ ", dirs = " ++ show dirs) $ case term of
    -- Variables: look up the type in the context
    Var s -> Right $ lookupType s ctx
    -- Universe
    Universe -> Right Universe
    -- Function application: the type of the function is inferred
    App fun arg -> do
        funTy <- inferType ctx dirs fun
        -- Handle restriction and partial types: `box` is used
        -- to put the resulting type in the eventual restriction
        -- or partial type
        let (funTy',box) = case funTy of
                Restr sys v   -> (v,makeRestr sys)
                Partial phi v -> (v,makePartial phi)
                otherwise     -> (funTy,curry snd)
            makeRestr :: System -> Value -> Value -> Value
            makeRestr sys val = foldRestr $ mapSys (`doApply` val) sys
            makePartial :: DisjFormula -> Value -> Value -> Value
            makePartial phi _ = foldPartial phi
        -- The type must be a ∏-type
        case funTy' of
            c@(Closure (Abst _ t _) ctx1) -> do
                checkType ctx dirs arg (eval ctx1 t)
                let argVal = eval ctx arg
                return $ box argVal (doApply c argVal)
            otherwise -> Left $
                "term '" ++ show fun ++ "' has type '" ++ show funTy
                    ++"' , which is not a product"
    -- First projection: the type of the argument is inferred
    Fst p -> do
        ty <- inferType ctx dirs p

        -- Handle restriction and partial types 
        let (ty',box) = case ty of
                Restr sys t   -> (t,makeRestr sys)
                Partial phi v -> (v,foldPartial phi)
                otherwise     -> (ty,id)
            makeRestr :: System -> Value -> Value
            makeRestr = foldRestr . mapSys doFst
        -- The type must be a ∑-type
        case ty' of
            Closure (Sigma _ t _) ctx1 -> do
                return $ box (eval ctx1 t)
            otherwise -> Left $
                "term '" ++ show term ++ "' has type '" ++ show ty
                    ++ "' , which is not a sum"
    -- Second projection: the type of the argument is inferred
    Snd p -> do
        ty <- inferType ctx dirs p

        --Handle restriction and partial types
        let (ty',box) = case ty of
                Restr sys t   -> (t,makeRestr sys)
                Partial phi v -> (v,foldPartial phi)
                otherwise     -> (ty,id)
            makeRestr :: System -> Value -> Value
            makeRestr = foldRestr . mapSys doSnd
        -- The type must be a ∑-type
        case ty' of
            c@(Closure Sigma{} ctx1) -> do
                return . box $ evalClosure c (doFst $ eval ctx p)
            otherwise -> Left $
                "term '" ++ show term ++ "' has type '" ++ show ty
                    ++ "' , which is not a sum"

    -- Coproduct eliminator: the type of the argument is inferred
    Split fam f1 f2 x -> do
        ty <- inferType ctx dirs x

        --Handle restriction and partial types
        let (ty',box) = case ty of
                Restr sys t   -> (t,makeRestr sys)
                Partial phi v -> (v,foldPartial phi)
                otherwise     -> (ty,id)
            famV = eval ctx fam
            makeRestr :: System -> Value -> Value
            makeRestr = foldRestr . mapSys (famV `doApply`)
        -- The type must be a coproduct
        case ty' of
            Sum{} -> do
                let sty@(Sum sty1 sty2) = readBack (keys ctx) ty'
                    var = newVar (keys ctx) (Ident "a")
                checkType ctx dirs fam
                    (Closure (Abst (Ident "_") sty Universe) ctx)
                checkType ctx dirs f1
                    (eval ctx $ Abst var sty1 (App fam (InL (Var var))))
                checkType ctx dirs f2
                    (eval ctx $ Abst var sty2 (App fam (InR (Var var))))
                return . box $ eval ctx (App fam x)
            otherwise -> Left $ "expected a sum type, got term '" ++ show x
                ++ "' of type '" ++ show ty ++ "' instead"
    -- Naturals
    Nat -> Right Universe
    Zero -> Right Nat
    Succ n -> do
        checkType ctx dirs n Nat
        Right Nat
    -- Induction for naturals
    Ind fam base step n -> do
        -- Check that `n` is a natural (eventually partial/restricted)
        nTyVal <- inferType ctx dirs n
        isNat n nTyVal
        -- Check that `fam` has type N -> U
        checkType ctx dirs fam (makeFunTypeVal Nat Universe)

        -- Handle restriction and partial types
        let box = case nTyVal of
                Restr sys Nat -> makeRestr sys
                Partial phi _ -> foldPartial phi
                Nat           -> id
            famV = eval ctx fam
            makeRestr :: System -> Value -> Value
            makeRestr = foldRestr . mapSys (famV `doApply`)

        -- Evaluate the type-family `fam`, checking that `base` has
        -- type `fam Z` 
        let tyVal  = eval ctx fam
            tyVal0 = doApply tyVal Zero
        checkType ctx dirs base tyVal0

        -- Checking that the "inductive step" `step` has type
        -- [n : nat] fam n -> fam (suc n)
        let varname = newVar (keys ctx) (Ident "n")
            var     = Var varname
            ctx'    = extend ctx varname (Decl Nat)
        checkType ctx dirs step (eval ctx'
            (Abst varname Nat
                (Abst (Ident "") (App fam var)
                    (App fam (Succ var)))
            )) -- [n : nat] fam n -> fam (suc n)
        
        return . box $ doApply tyVal (eval ctx n)
    -- Interval endpoints
    I0 -> Right I
    I1 -> Right I
    -- Composition
    Comp fam phi@(Disj df) i0 u b i -> do
        -- Checking the type-family `fam`, point `i_0` and formula `phi`
        checkType ctx dirs fam (makeFunTypeVal I Universe) -- I -> U
        -- TODO Check that the type is fibrant --var `notElem` concatMap vars (keys sysR)
        checkType ctx dirs i0 I
        checkDisjFormula ctx phi
        -- Checking that `u` has the correct type
        let var = newVar (keys ctx) (Ident "_i")
        --let ctx' = extend ctx var (Decl I)
        checkType ctx dirs u (eval ctx
            (Abst var I (Partial phi (App fam (Var var)))))
        -- Checking that `b` has type `[phi -> u i0](fam i0)`
        checkType ctx dirs b $
            eval ctx (Restr (map (\psi -> (psi,App u i0)) df) (App fam i0))
        -- Return the evaluated type, without the restriction if it's empty
        let sys = getCompSys phi i0 u b i
        return $ eval ctx $ (if null sys then id else Restr sys) (App fam i)

    -- Failed type inference
    _  -> Left $ "don't know how to infer type of '" ++ show term ++ "'"

-- Check if a term has Nat type
isNat :: Term -> Value -> Either ErrorString ()
isNat _ Nat           = Right ()
isNat _ (Restr _ Nat) = Right ()
isNat _ (Partial _ Nat) = Right ()
isNat t v             = Left $ "expected type nat, got term '" ++ show t ++
    "' of type '" ++ show v ++ "' instead"

-- Utility function to get type values of the form A -> B
makeFunTypeVal :: Term -> Term -> Value
makeFunTypeVal ty e = eval emptyCtx (Abst (Ident "") ty e)

-- Check the type of a term under a conjunction
-- If the conjunction is false, type-check is trivially true
checkTypePartialConj :: ConjFormula -> Ctx -> DirEnv -> Term -> Value -> Either ErrorString ()
--checkTypePartialConj conj ctx dirs e v = myTrace ("[checkTypePartialConj] conj = " ++ show conj ++ ", dirs' = " ++ show (addConj dirs conj) ++ ", e = " ++ show e ++ ", v = " ++ show v) $ do
checkTypePartialConj conj ctx dirs e v = do
    let dirs' = addConj dirs conj
    unless (inconsistent dirs') $
        checkType ctx dirs' e v

-- Check the type of a term under a disjunction, that is
-- under each conjunction
checkTypePartialDisj :: DisjFormula -> Ctx -> DirEnv -> Term -> Value -> Either ErrorString ()
--checkTypePartialDisj (Disj df) ctx dirs e v = myTrace ("[checkTypePartialDisj] disj = " ++ show (Disj df) ++ ", e = " ++ show e ++ ", v = " ++ show v) $
checkTypePartialDisj (Disj df) ctx dirs e v = 
    mapM_ (\conj -> checkTypePartialConj conj ctx dirs e v) df

-- Check the type of a term agains a given type
-- The type must be a value (i.e. in β-normal form)
checkType :: Ctx -> DirEnv -> Term -> Value -> Either ErrorString ()
checkType ctx dirs term v = myTrace ("[checkType]<= term = " ++ show term ++ ", v = " ++ show v ++ ", ctx = " ++ showCtx ctx ++ ", dirs = " ++ show dirs) $ case (term,v) of
--checkType ctx dirs term v = case (term,v) of
    -- Let-definition
    (TDef (s,t,e) t',_) -> do
        checkType ctx dirs t Universe
        checkType (extend ctx s (Decl t)) dirs e (eval ctx t)
        checkType (extend ctx s (Def t e)) dirs t' v
    -- ∏-type former
    (Abst s t e,Universe) -> do
        -- Handle also path types
        unless (t == I) $ checkType ctx dirs t Universe
        checkType (extend ctx s (Decl t)) dirs e Universe
    -- ∑-type former
    (Sigma s t e,Universe) -> do
        checkType ctx dirs t Universe
        checkType (extend ctx s (Decl t)) dirs e Universe
    -- λ- or ∏- abstraction
    (Abst s t e,Closure (Abst _ t1 _) ctx1) -> do
        -- Handle also I-abstractions
        unless (t == I) $ checkType ctx dirs t Universe
        let tVal  = eval ctx t
            t1Val = eval ctx1 t1
        unless (conv (keys ctx) dirs tVal t1Val) $
            Left $ "type '" ++ show tVal ++ "' is not convertible to type '"
                ++ show t1Val ++ "' (while checking term '" ++ show (Abst s t e)
                ++ "' against type '" ++ show v ++ "')"
        -- Introduce a fresh variable and check the body
        let var   = newVar (keys ctx1) s
            e1Val = doApply v (Neutral (Var var) t1Val)
            ctx'  = if s == var then
                extend ctx s (Decl t)
            else
                extend (extend ctx s (Decl t)) s (Val (Neutral (Var var) tVal))
        checkType ctx' dirs e e1Val
    -- ∑-type constructor (pair)
    (Pair p1 p2,Closure (Sigma _ t1 _) ctx1) -> do
        let t1Val = eval ctx1 t1
            e1Val = evalClosure v (eval ctx p1)
        checkType ctx dirs p1 t1Val
        checkType ctx dirs p2 e1Val
    -- Coproduct type former
    (Sum ty1 ty2,Universe) -> do
        checkType ctx dirs ty1 Universe
        checkType ctx dirs ty2 Universe
    -- ∑-type left injection
    (InL t1,Sum ty1 _) -> do
        checkType ctx dirs t1 ty1
    -- ∑-type right injection
    (InR t2,Sum _ ty2) -> do
        checkType ctx dirs t2 ty2
    -- Restriction type
    (e,Restr sys ty) -> do
        let eVal = eval ctx e
            phi  = getSystemFormula sys
        checkType ctx dirs e ty
        unless (convPartialDisj (keys ctx) phi dirs eVal (Sys sys)) $
            Left $ "term '" ++ show e ++ "' does not agree with '" ++
                show (Sys sys) ++ "' on " ++ show phi
    -- System
    (Sys sys,Partial phi ty) -> do
        let psis = keys sys
        mapM_ (checkConjFormula ctx) psis
        unless (eqFormulas dirs phi (Disj psis)) $
            Left $ show phi ++ " is not logically equivalent to "
                ++ show (Disj psis)

        mapM_ (\(psi,t) -> checkTypePartialConj psi ctx dirs t ty) sys
        let eq_check = all (\((psi1,t1),(psi2,t2)) ->
                convPartialConj (keys ctx) (psi1 `meet` psi2) dirs
                    (eval ctx t1) (eval ctx t2)
                    ) [(x1,x2) | x1 <- sys, x2 <- sys, x1 /= x2]
        unless eq_check $
            Left "values are not adjacent"
    -- Partial type former
    (Partial phi ty,Universe) -> do
        checkDisjFormula ctx phi
        checkType ctx dirs ty Universe
    -- Restriction type former
    (Restr sys ty,Universe) -> do
        checkType ctx dirs ty Universe
        let tyVal = eval ctx ty
            phi   = getSystemFormula sys
        checkDisjFormula ctx phi
        -- Check the elements in the system
        mapM_ (\(conj,t) -> checkTypePartialConj conj ctx dirs t tyVal) sys
    -- If no other rule match, try inferring the type and
    -- check if it's compatible
    otherwise -> do
        ty <- inferType ctx dirs term
        -- Check for sub-typing: `v` more general than `ty` 
        myTrace ("[checkType] inferred type of " ++ show term ++ " = " ++ show ty ++ ", ctx = " ++ showCtx ctx) $
            unless (compTypes (keys ctx) dirs ty v) $
                Left $ "type '" ++ show v ++ "' expected, got term '" ++ show term
                    ++ "' of type '" ++ show ty ++ "' instead"

-- Check compatibility (subtyping) between two partial or restriction types,
-- as specified by the typing rules: `v1` more general than `v2`
compTypes :: [Ident] -> DirEnv -> Value -> Value -> Bool
compTypes used dirs v1 v2 = 
    -- Split the type and get eventual partial type formulas
    -- (by default `True` in the other cases)
    let (iphi,ity) = split v1
        (vphi,vty) = split v2
        syscheck   = case (v1,v2) of
            (Restr isys _,Restr vsys _) ->
                convPartialDisj used (getSystemFormula vsys)
                    dirs (Sys isys) (Sys vsys)
            otherwise -> True
        -- Check sub-typing compatibility for restrictions, for the base
        -- types and for formulas in the case of partial types
        in syscheck && conv used dirs ity vty && impDisj dirs vphi iphi

-- Check that the variables of the conjunctive formula are in the context
checkConjFormula :: Ctx -> ConjFormula -> Either ErrorString ()
checkConjFormula ctx cf = do
    let dom     = keys ctx
        support = vars cf
    unless (all (`elem` dom) support) $
        Left $ "formula '" ++ show cf ++ "' contains undeclared names"

-- Check that the variables of the disjunctive formula are in the context
checkDisjFormula :: Ctx -> DisjFormula -> Either ErrorString ()
checkDisjFormula ctx (Disj df) = mapM_ (checkConjFormula ctx) df
