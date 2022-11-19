{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Eval where

import Data.List (intercalate,nub,find)
import Data.Foldable (foldrM)
import Data.Maybe (mapMaybe,fromJust)

import Ident
import Interval
import CoreCTT

import Debug.Trace

-- For debug purposes only
debug :: Bool
debug = False

myTrace :: String -> a -> a
myTrace s x = if debug then trace s x else x

-- Retrieve the evaluated type of a variable from the context
lookupType :: Ident -> Ctx -> Value
lookupType s [] = error $ "[lookupType] got unknown identifier " ++ show s
lookupType s ((s',entry):ctx) = if s == s' then
        case entry of
            Decl ty     -> eval ctx ty
            Def  ty _   -> eval ctx ty
            Val _       -> lookupType s ctx
    else
        lookupType s ctx

-- Evaluate a term in the given context
eval :: Ctx -> Term -> Value
eval ctx term = myTrace ("[eval] " ++ show term ++ ", ctx = " ++ showCtx ctx) $ case term of
--eval ctx term = case term of
    Var s -> case lookup s ctx of
        Nothing          -> error $ "[eval] not found var `" ++ show s ++ "` in ctx"
        Just (Val v)     -> v
        Just (Decl ty)   -> simplNeutralValue $ Neutral (Var s) (eval ctx ty)
        Just (Def _ e)   -> eval ctx e
    Universe           -> Universe
    TDef (s,t,e) t'    -> eval (extend ctx s (Def t e)) t' 
    Abst{}             -> Closure term ctx
    App e1 e2          -> doApply (eval ctx e1) (eval ctx e2)
    Sigma{}            -> Closure term ctx
    Pair t1 t2         -> Pair (eval ctx t1) (eval ctx t2)
    Fst t              -> doFst (eval ctx t)
    Snd t              -> doSnd (eval ctx t)
    Sum ty1 ty2        -> Sum (eval ctx ty1) (eval ctx ty2)
    InL t1             -> InL (eval ctx t1)
    InR t2             -> InR (eval ctx t2)
    Split ty f1 f2 x   ->
        doSplit (eval ctx ty) (eval ctx f1) (eval ctx f2) (eval ctx x)
    Nat                -> Nat
    Zero               -> Zero
    Succ t             -> Succ (eval ctx t)
    Ind ty base step n -> doInd (eval ctx ty) (eval ctx base)
        (eval ctx step) (eval ctx n)
    I  -> I
    I0 -> I0
    I1 -> I1
    Sys sys               -> evalSystem ctx sys
    Partial phi t         -> foldPartial (evalDisjFormula ctx phi) (eval ctx t)
    Restr sys t           -> foldRestr (evalRestrSystem ctx sys) (eval ctx t)
    Comp fam phi i0 u b i -> doComp ctx fam phi i0 u b i
    -- Already evaluated term (used solely for `doComp`)
    TermV v               -> v
    otherwise             -> error $ "[eval] got " ++ show term

-- Evaluate a conjunctive formula
evalConjFormula :: Ctx -> ConjFormula -> Maybe ConjFormula
evalConjFormula ctx conj = myTrace ("[evalConjFormula] " ++ show conj ++ " => " ++ show conj') $ conj'
    where
        -- Get the bindings which concern the formula's variables
        entries' = filter (\(s,_) -> s `elem` vars conj) (getBindings ctx)
        -- Get only the last one of each binding
        entries  = map (\s -> fromJust $ find (\(s',_) -> s' == s) entries')
            (nub $ keys entries')
        -- Get the renamings from the entries (i.e. the ones of the form i -> j)
        renamings = concatMap (\case { (s,Neutral (Var s') I) -> [(s,s')] ;
              _ -> []}) entries
        -- Apply renamings to the conjuction
        renamedConj = foldr substConj conj renamings
        -- Apply value substitutions to the renamed conjuction
        vals        = filter (\(_,v) -> v == I0 || v == I1) entries
        conj'       = foldrM evalConj renamedConj vals

-- Evaluate a single conjuction by replacing `s` with 0
-- Returns `Nothing` if the resulting formula is false
evalConj :: (Ident,Value) -> ConjFormula -> Maybe ConjFormula
evalConj (s,I0) conj@(Conj cf) =
    if conjToDirEnv conj `makesTrueAtomic` Eq1 s -- Inconsistent cases
        || inconsistent (conjToDirEnv conj') then
        Nothing
    else
        Just conj' -- Substitute into each atomic formula
    where
        conj' = Conj . nub $ concatMap (\case
            Eq0 s' | s == s' -> [];
            Diag s1 s2 -> if s == s1 && s1 == s2 then []
                else [if s == s1 then Eq0 s2
                    else if s == s2 then Eq0 s1 else Diag s1 s2];
            af -> [af]) cf

-- Same as before, now replacing `s` with 1
evalConj (s,I1) conj@(Conj cf) = 
    if conjToDirEnv conj `makesTrueAtomic` Eq0 s -- Inconsistent cases
        || inconsistent (conjToDirEnv conj') then
        Nothing
    else
        Just conj' -- Substitute into each atomic formula
    where
        conj' = Conj . nub $ concatMap (\case
            Eq1 s' | s == s' -> [];
            Diag s1 s2 -> if s == s1 && s1 == s2 then []
               else [if s == s1 then Eq1 s2
                   else if s == s2 then Eq1 s1 else Diag s1 s2];
            af -> [af]) cf

-- Evaluate a disjunctive formula, by first checking if it contains
-- a true conjunction
evalDisjFormula :: Ctx -> DisjFormula -> DisjFormula
evalDisjFormula ctx (Disj df) = if Conj [] `elem` df then
        fTrue
    else -- Otherwise evaluate each conjunction, disregarding the false ones
        Disj $ mapMaybe (evalConjFormula ctx) df

-- Simplify cascading restriction types into one restriction type
-- E.g. [psi_1 -> t_1]([psi_2 -> t_2]([psi_3 -> t_3]A)) becomes
-- [psi_1 -> t_1 | psi_2 -> t_2 | psi_3 -> t_3]A
foldRestr :: System -> Value -> Value
foldRestr sys0 v0 = -- If the system if empty, return just the type
    if null sys then v else Restr (nub sys) v
    where
        (sys,v) = foldRestr' sys0 v0
        foldRestr' :: System -> Value -> (System,Value)
        foldRestr' sys v = case v of
            Restr sys' v' -> foldRestr' (sys ++ sys') v'
            otherwise     -> (sys,v)

-- Simplify cascading partial types into one partial type
-- E.g. [phi_1]([phi_2]([phi_3]A)) becomes
-- [phi /\ psi_2 /\ psi_3]A
foldPartial :: DisjFormula -> Value -> Value
foldPartial (Disj df0) v0 = -- If the formula is true, return just the type
    if makesTrueDisj emptyDirEnv (Disj df) then
        v
    else -- `nub` removes duplicate formulas
        Partial (Disj $ nub df) v
    where
        (df,v) = foldPartial' df0 v0
        foldPartial' :: [ConjFormula] -> Value -> ([ConjFormula],Value)
        foldPartial' df v = case v of
            Partial (Disj df') v' -> foldPartial' (dnf df' df) v'
            otherwise             -> (df,v)
        -- Get the conjuction of the two disjunctive formulas,
        -- in disjunctive normal form
        dnf :: [ConjFormula] -> [ConjFormula] -> [ConjFormula]
        dnf df1 df2 = [cf1 `meet` cf2 | cf1 <- df1, cf2 <- df2] 

-- Evaluate the system of a restriction type,
-- by dropping false formulas
evalRestrSystem :: Ctx -> System -> System
evalRestrSystem ctx sys =
    concatMap (\(phi,t) -> evalConjFormula' phi (eval ctx t)) sys
    where
        -- Need to handle the case of false formulas
        evalConjFormula' phi v = case evalConjFormula ctx phi of
            Nothing -> []
            Just cf -> [(cf,v)]

-- Evaluate a system and eventually simplify it (recursively)
-- if there is a true formula
evalSystem :: Ctx -> System -> Value
evalSystem ctx sys =
    case foldrM evalConjFormula' [] sys of
        Left val   -> val      -- System has been simplified
        Right sys' -> Sys sys' -- There were no true formulas
    where
        -- The following function gets the new pair (formula,term) and the
        -- part of the system already evaluated. If the new formula is true,
        -- it returns the given value using `Left`; otherwise, the new evaluated
        -- system is returned inside `Right`
        evalConjFormula' :: (ConjFormula,Term) -> System -> Either Value System
        evalConjFormula' (psi,t) sys' = case evalConjFormula ctx psi of
            Nothing        -> Right sys         -- False formula: nothing to add
            Just (Conj []) -> Left $ eval ctx t -- True formula, return the value
            Just cf        -> Right $ (cf,eval ctx t) : sys' -- Otherwise, append

-- Simplify a neutral value if the type is a restriction type with a true
-- formula, otherwise do nothing.
-- `simplNeutralValue` is used only in evaluation.
simplNeutralValue :: Value -> Value
simplNeutralValue neu@(Neutral _ ty) = case ty of
    Restr sys _ | any (isTrueConj . fst) sys ->
        snd . fromJust $ find (isTrueConj . fst) sys
    otherwise -> neu

-- Split the Abst/Sigma constructor and the arguments from a value
-- inside a closure
extract :: Value -> (Ident -> Term -> Term -> Value,Ident,Term,Term)
extract (Abst  s t e) = (Abst ,s,t,e)
extract (Sigma s t e) = (Sigma,s,t,e)
extract v             = error $ "[extract] got " ++ show v

-- Evaluate a closure, extending the context by assigning the variable to
-- the given value. In case of non-dependent abstractions, i.e. empty
-- variable, we don't need to extend the context
evalClosure :: Value -> Value -> Value
evalClosure (Closure (Abst  s t e) ctx) arg =
    eval (if s == Ident "" then ctx else extend ctx s (Val arg)) e
evalClosure (Closure (Sigma s t e) ctx) arg =
    eval (if s == Ident "" then ctx else extend ctx s (Val arg)) e
{-evalClosure (Closure (Abst  s t e) ctx) arg =
    eval (if s == Ident "" then ctx else extend (case arg of Neutral (Var (Ident s')) _ -> extend ctx (Ident s') (Decl t); _ -> ctx) s (Val arg)) e
evalClosure (Closure (Sigma s t e) ctx) arg =
    eval (if s == Ident "" then ctx else extend (case arg of Neutral (Var (Ident s')) _ -> extend ctx (Ident s') (Decl t); _ -> ctx) s (Val arg)) e-}
evalClosure v arg = error $ "[evalClosure] got non-closure " ++ show v
    ++ " applied to " ++ show arg

-- Handler of `App` (function application, i.e. ∏-type eliminator)
doApply :: Value -> Value -> Value
-- Standard case: do β-reduction
doApply fun@(Closure Abst{} ctx) arg = evalClosure fun arg
-- Restricted abstraction case, which requires to apply the function
-- inside the restriction too
doApply (Restr sys fun@Closure{}) arg = foldRestr sys' (doApply fun arg)
    where sys' = mapSys (`doApply` arg) sys
-- Standard neutral case
doApply fun@(Neutral _ fty@Closure{}) arg =
    simplNeutralValue $ Neutral (App fun arg) (doApply fty arg)
-- Restricted neutral case
doApply fun@(Neutral _ (Restr sys cl@Closure{})) arg =
    simplNeutralValue $ Neutral (App fun arg) (foldRestr sys' (doApply cl arg))
    where sys' = mapSys (`doApply` arg) sys
-- System case
doApply (Sys sys) arg = Sys $ mapSys (`doApply` arg) sys
-- Partial type case
doApply fun@(Neutral _ (Partial phi cl@Closure{})) arg =
    Neutral (App fun arg) (foldPartial phi (doApply cl arg))
doApply v arg = error $ "[doApply] got " ++ show v ++ ", " ++ show arg

-- Handler of `Fst` (i.e. ∑-type first projection)
doFst :: Value -> Value
doFst v = case v of
    -- Standard case: do β-redution
    Pair v1 _ -> v1
    -- Stardard neutral case; need to compute the type
    Neutral _ (Closure (Sigma _ t _) ctx) -> simplNeutralValue $ Neutral (Fst v)
        (eval ctx t)
    -- Restricted neutral case
    Neutral x (Restr sys cl@(Closure (Sigma _ t _) ctx)) ->
        simplNeutralValue $ Neutral (Fst (Neutral x cl))
            (foldRestr sys' (eval ctx t))
        where sys' = mapSys doFst sys
    -- System case
    Sys sys -> Sys $ mapSys doFst sys
    -- Partial type case
    Neutral x (Partial phi cl@(Closure (Sigma _ t _) ctx)) ->
        Neutral (Fst (Neutral x cl)) (foldPartial phi (eval ctx t))
    otherwise -> error $ "[doFst] got " ++ show v

-- Handler of `Snd` (i.e. ∑-type second projection)
doSnd :: Value -> Value
doSnd v = case v of
    -- Standard case: do β-redution
    Pair _ v2 -> v2
    -- Stardard neutral case; need to compute the type
    Neutral _ ty@(Closure Sigma{} _) -> simplNeutralValue $ Neutral (Snd v)
        (evalClosure ty (doFst v))
    -- Restricted neutral case
    Neutral x (Restr sys cl@(Closure Sigma{} _)) ->
        simplNeutralValue $ Neutral (Snd (Neutral x cl))
            (foldRestr sys' (evalClosure cl (doFst (Neutral x cl))))
        where sys' = mapSys doSnd sys
    -- System case
    Sys sys -> Sys $ mapSys doSnd sys
    -- Partial type case
    Neutral x (Partial phi cl@(Closure Sigma{} _)) ->
        Neutral (Snd (Neutral x cl))
            (foldPartial phi (evalClosure cl (doFst (Neutral x cl))))
    otherwise -> error $ "[doSnd] got " ++ show v

-- Handler of `Split` (i.e. (+)-type eliminator)
doSplit :: Value -> Value -> Value -> Value -> Value
doSplit fam f1 f2 x = case x of
    -- Standard cases (left/right injection): do β-redution
    InL x1 -> doApply f1 x1
    InR x2 -> doApply f2 x2
    -- Stardard neutral case; need to compute the type
    Neutral _ (Sum _ _) -> simplNeutralValue $
        Neutral (Split fam f1 f2 x) (doApply fam x)
    -- Restricted neutral case
    Neutral _ (Restr sys (Sum ty1 ty2)) -> simplNeutralValue $
        Neutral (Split fam f1 f2 (Neutral x (Sum ty1 ty2)))
            (foldRestr sys' (doApply fam x))
        where sys' = mapSys (doSplit fam f1 f2) sys
    -- System case
    Sys sys -> Sys $ mapSys (doSplit fam f1 f2) sys
    -- Partial type case
    Neutral _ (Partial phi (Sum ty1 ty2)) ->
        Neutral (Split fam f1 f2 (Neutral x (Sum ty1 ty2)))
            (foldPartial phi (doApply fam x))
    otherwise -> error $ "[doSplit] got " ++ show x

-- Handler of `Ind` (i.e. Nat eliminator)
doInd :: Value -> Value -> Value -> Value -> Value
doInd fam base step m = case m of
    -- Standard base and inductive cases: do β-redution
    Zero     -> base
    Succ n'  -> doApply fun prev
        where
            fun = doApply step n'
            prev = doInd fam base step n'
    -- Stardard neutral case; need to compute the type
    Neutral n Nat -> simplNeutralValue $ Neutral (Ind fam base step n)
        (doApply fam (Neutral n Nat))
    -- Restricted neutral case
    Neutral n (Restr sys Nat) -> 
        simplNeutralValue $ Neutral (Ind fam base step n)
            (foldRestr sys' (doApply fam (Neutral n Nat)))
        where sys' = mapSys (doInd fam base step) sys
    -- System case
    Sys sys -> Sys $ mapSys (doInd fam base step) sys
    -- Partial type case
    Neutral n (Partial phi Nat) -> 
        Neutral (Ind fam base step n)
            (foldPartial phi (doApply fam (Neutral n Nat)))
    otherwise -> error $ "[doInd] got " ++ show m

-- Utility function to handle eventually empty strings
ifEmpty :: Ident -> String -> Ident
ifEmpty (Ident "") s = Ident s
ifEmpty i          _ = i


{- Handler of composition (which is not an eliminator!).
The problem with composition is that we need to pattern-match inside
a closure (`famV`); when calling `doComp` recursively, we must
read-back terms, but we cannot do so for the type family (otherwise
it would then be evaluated under the general context and not in its own).
Therefore, we introduce a wrapper `TermV` which makes it possible to
include values (closures) inside terms, so that when the whole term is
evaluated, the closure is evaluated with the right context. The wrapper
`TermV` is used solely in the evaluation of composition.
-}
doComp :: Ctx -> Term -> DisjFormula -> Term -> Term -> Term -> Term -> Value
--doComp ctx fam phi i0 u b i = myTrace ("[doComp-?] " ++ show (Comp fam phi i0 u b i) ++ ", ctx = " ++ showCtx ctx) $
doComp ctx fam phi i0 u b i = 
    if isTrue phiV then  -- 1° trivial case: `phi` is True
        doApply uV iV
    else if i0V == iV then -- 2° trivial case: i = i_0 (no need for `conv`)
        bV
    else
        -- Fresh variables: `var` to pattern-match the type family
        --                  `var2` to handle the partially defined `u` 
        let var  = newVar (keys ctx) (Ident "_i")
            var2 = case uV of
                Closure (Abst v _ _) _ -> newVar (keys ctx) (ifEmpty v "j")
            emptySys = Abst (Ident "") I (Sys [])
            ctxOf v = case v of Closure _ gctx -> gctx
        -- Evaluate the type-family, pattern-matching inside the closure
        in case doApply famV (Neutral (Var var) I) of
            -- ∏-type `[x:ty]e`, with `ty` a type
            cl@(Closure (Abst x ty e) ctx') | eval ctx' ty /= I -> myTrace ("[doComp-∏] " ++ show (Comp fam phi i0 u b i) ++ ", ctx = " ++ showCtx ctx ++ ", ctx' = " ++ showCtx ctx') $
                Closure (Abst yi tyc comp) ctx
                where
                    -- Variable of the target type `tyc`, i.e. `ty(i)`
                    yi   = newVar (var : var2 : keys ctx) (ifEmpty x "u")
                    -- Transport of that variable at `i0` and `var'
                    yt0  = Comp (TermV fam1) fFalse i emptySys (Var yi) i0
                    yt   = Comp (TermV fam1) fFalse i emptySys (Var yi) (Var var)
                    -- Resulting composition
                    comp = Comp (TermV fam2) phi i0 u' (App b yt0) i
                    -- Type families
                    fam1 = Closure (Abst var I ty) ctx'
                    fam2 = Closure (Abst var I (App (Abst x ty e) yt)) ctx'
                    -- Target type: `ty(i)`
                    tyc  = readBack (keys ctx) $ case doApply famV iV of
                        Closure (Abst _ ty' _) ctx'' -> eval ctx'' ty'
                    -- Apply functions to `yt` inside `u`
                    u' = case isFalse phiV of
                        True  -> u
                        False -> Abst var2 I (App (App u (Var var2)) yt)
            -- ∏-type `[x:I]e`     
            cl@(Closure (Abst x ty e) ctx') | eval ctx' ty == I -> myTrace ("[doComp-∏I] " ++ show (Comp fam phi i0 u b i) ++ ", ctx = " ++ showCtx ctx ++ ", ctx' = " ++ showCtx ctx') $
                Closure (Abst x' I comp) ctx
                where
                    -- Fresh nterval variable
                    x'   = newVar (var : var2 : keys ctx) (ifEmpty x "i")
                    -- Resulting composition
                    comp = Comp (TermV fam') phi i0 u' (App b (Var x')) i
                    fam' = Closure (Abst var I e) ctx'
                    -- Apply functions to `x'` inside `u`
                    u'   = case isFalse phiV of
                        True  -> u
                        False -> Abst var2 I (App (App u (Var var2)) (Var x'))
            -- ∑-type `<x:ty>e`
            cl@(Closure (Sigma _ ty _) ctx') -> myTrace ("[doComp-∑] " ++ show (Comp fam phi i0 u b i) ++ ", ctx = " ++ showCtx ctx ++ ", ctx' = " ++ showCtx ctx') $ Pair c1 c2
                where
                    -- Composition on each pair
                    c1  = doComp ctx (TermV fam1) phi i0 u0 (Fst b) i
                    c2  = doComp ctx (TermV fam2) phi i0 u1 (Snd b) i
                    -- The type family of `c2` needs the comp. on the first component
                    fam1 = Closure (Abst var I ty) ctx'
                    fam2 = Closure (Abst var I ty2) ctx'
                    ty2 = readBack (keys ctx) $ evalClosure cl c1'
                    c1' = doComp ctx (TermV fam1) phi i0 u0 (Fst b) (Var var)
                    -- Apply projections inside `u`
                    (u0,u1) = case isFalse phiV of
                        True  -> (u,u)
                        False -> (Abst var2 I (Fst (App u (Var var2))),
                                  Abst var2 I (Snd (App u (Var var2))))
            -- Coproduct `ty1 + ty2`
            Sum ty1 ty2 -> myTrace ("[doComp-Coproduct] " ++ show (Comp fam phi i0 u b i) ++ ", ctx = " ++ showCtx ctx) $
                -- If `b` is neutral, the result is neutral
                case bV of
                    Neutral{} -> doNeutralComp
                    otherwise -> inj comp
                where (inj,bV',ty) = case bV of
                      -- Extract the injection, the inner value and the type
                        InL b1 -> (InL,b1,ty1)
                        InR b2 -> (InR,b2,ty2)
                      -- Resulting composition
                      comp = doComp ctx (TermV fam') phi i0 u' b' i
                      b'   = readBack (keys ctx) bV'
                      fam' = Closure (Abst var I ty') (ctxOf famV)
                      ty'  = readBack (keys (ctxOf famV)) ty
                      -- Remove the outer injections from `u`
                      u' = case isFalse phiV of
                        True  -> u
                        False -> Abst var2 I (readBack (keys ctx) sys')
                        where app  = App u (Var var2)
                              sys' = case eval (extend ctx var2 (Decl I)) app of
                                Sys sysV -> Sys $ map (\case {(psi,InL q) -> (psi,q);
                                    (psi,InR q) -> (psi,q)}) sysV
                                InL q    -> q
                                InR q    -> q
            -- Naturals
            Nat -> myTrace ("[doComp-Nat] " ++ show (Comp fam phi i0 u b i) ++ ", ctx = " ++ showCtx ctx) $ case bV of
                Zero    -> Zero
                Succ b' -> Succ $ doComp ctx fam phi i0 u' b' i
                    -- Remove the outer `S` from `u`
                    where u' = case isFalse phiV of
                            True  -> u
                            False -> Abst var2 I (readBack (keys ctx) sys')
                            where app  = App u (Var var2)
                                  sys' = case eval (extend ctx var2 (Decl I)) app of
                                    Sys sysV -> Sys $
                                        map (\case (psi,Succ m) -> (psi,m)) sysV
                                    Succ m   -> m
                Neutral{} -> doNeutralComp
                _ -> error $ "[doComp-Nat] got " ++ show bV
            -- Partial type `[phi]ty`
            Partial (Disj df) tyV -> myTrace ("[doComp-Partial] " ++ show (Comp fam phi i0 u b i) ++ ", ctx = " ++ showCtx ctx) $
                    if var `elem` vars (Disj df) then
                        error $ "Type family '" ++ show (readBack (keys ctx) famV)
                            ++ "' is not fibrant"
                    else
                        sysComp
                where
                    -- Type family (without the formula)
                    fam' = Closure (Abst var I ty') (ctxOf famV)
                    ty'  = readBack (keys (ctxOf famV)) tyV
                    -- Obtain a system will a composition for each conjunction
                    -- If the system has just one value, return it
                    sysCompVals = map helperComp df
                    sysComp = if length df == 1 then
                            head sysCompVals
                        else
                            Sys $ zip df sysCompVals
                    -- Do composition on each conjunction
                    helperComp :: ConjFormula -> Value
                    helperComp conj = doComp (addConjBindings ctx conj)
                        (TermV fam') phi i0 u (b' conj) i
                    -- Eventually simplify `b` (as it may have already been evaluated)
                    b' :: ConjFormula -> Value
                    b' conj = readBack (keys ctx) $ case bV of
                        -- A formula is true
                        Sys sysU | any ((conjToDirEnv conj `makesTrueConj`) . fst) sysU
                          -> snd . fromJust $
                            find ((conjToDirEnv conj `makesTrueConj`) . fst) sysU
                        _ -> bV
            -- Restriction type `[psi -> w]ty`
            Restr sysR tyV -> myTrace ("[doComp-Restr] " ++ show (Comp fam phi i0 u b i) ++ ", ctx = " ++ showCtx ctx) $
                    if var `elem` concatMap vars (keys sys) then
                        error $ "Type family '" ++ show (readBack (keys ctx) famV)
                            ++ "' is not fibrant"
                    else
                        doComp ctx (TermV fam') formula i0 u' b i
                where
                    -- Get the new formula
                    formula = Disj $ phi' ++ psis
                    phi' = case phi of Disj ff -> ff 
                    psis = keys sysR
                    -- Type family (without the restriction)
                    fam' = Closure (Abst var I ty') (ctxOf famV)
                    ty'  = readBack (keys (ctxOf famV)) tyV
                    u'   = Abst var2 I (Sys sys')
                    -- Concatenate the two systems
                    sys' = map (\conj -> (conj,App u (Var var2))) phi'
                        ++ case doApply famV (Neutral (Var var2) I) of
                            Restr sys'' _ ->
                                mapSys (TermV . readBack (keys ctx)) sys''

            -- Neutral type family; the result of the composition is neutral too
            otherwise -> myTrace ("[doComp-neutral] " ++
                show (Comp fam phi i0 u b i)) $ doNeutralComp
    where
        -- Values computed for each argument (remember that Haskell is lazy!)
        famV = myTrace ("[doComp - fam = " ++ show fam ++ "]" ++ show (Comp fam phi i0 u b i)) $ eval ctx fam
        phiV = evalDisjFormula ctx phi
        i0V  = eval ctx i0
        uV   = myTrace ("[doComp - u = " ++ show u ++ "]" ++ show (Comp fam phi i0 u b i)) $ eval ctx u
        bV   = myTrace ("[doComp - b = " ++ show b ++ "]" ++ show (Comp fam phi i0 u b i)) $ eval ctx b
        iV   = eval ctx i
        -- Compute the type of the composition, and prepare the neutral value
        doNeutralComp = simplNeutralValue $
            Neutral (Comp famV phiV i0V uV bV iV) (eval ctx compTy)
        sys    = getCompSys phi i0 u b i
        compTy = Restr sys (App fam i)

-- Get the system of the restriction type of a composition
getCompSys :: DisjFormula -> Term -> Term -> Term -> Term -> System
getCompSys (Disj df) i0 u b i = eq ++ map (\conj -> (conj,App u i)) df
    where
        -- Extract the variables from eventual values
        i'  = case i  of { Neutral (Var x) _ -> Var x ; _ -> i  }
        i0' = case i0 of { Neutral (Var x) _ -> Var x ; _ -> i0 }
        -- Translate `i = i0` into a real formula
        eq  = case (i0',i') of
            (I0,I0)        -> [(Conj [],b)]
            (I0,I1)        -> []
            (I0,Var s)     -> [(Conj [Eq0 s],b)]
            (Var s,I0)     -> [(Conj [Eq0 s],b)]
            (Var s,I1)     -> [(Conj [Eq1 s],b)]
            (Var s,Var s') -> [(Conj [Diag s s'],b)]
            (I1,I0)        -> []
            (I1,I1)        -> [(Conj [],b)]
            (I1,Var s)     -> [(Conj [Eq1 s],b)]
            _              -> error $ "[getCompSys] got " ++ show (i,i0)

-- Read-back function which converts values back into terms
-- The first argument is the list of already used names
-- The only non-trivial case is that of closures
readBack :: [Ident] -> Value -> Term
readBack used val = case val of
    App fun arg -> App (readBack used fun) (readBack used arg)
    Succ v -> Succ (readBack used v)
    Fst v -> Fst (readBack used v)
    Snd v -> Snd (readBack used v)
    Pair v1 v2 -> Pair (readBack used v1) (readBack used v2)
    Sum v1 v2 -> Sum (readBack used v1) (readBack used v2)
    InL v -> InL (readBack used v)
    InR v -> InR (readBack used v)
    Split ty f1 f2 x -> Split (readBack used ty) (readBack used f1)
        (readBack used f2) (readBack used x)
    Sys sys -> Sys $ mapSys (readBack used) sys
    Partial phi ty -> foldPartial phi (readBack used ty)
    Restr sys ty -> foldRestr (mapSys (readBack used) sys) (readBack used ty)
    Ind ty b e n -> Ind (readBack used ty) (readBack used b) (readBack used e)
        (readBack used n)
    Comp fam phi i0 u b i -> Comp (readBack used fam) phi (readBack used i0)
        (readBack used u) (readBack used b) (readBack used i)
    -- Closure case: first evaluate the body with a fresh name, then read-back
    cl@(Closure f ctx) -> let
        -- `constr` is `Abst` or `Sigma`
        (constr,s,t,e) = extract f
        s'      = newVar used s
        eVal    = evalClosure cl (Neutral (Var s') (eval ctx t))
        e'      = readBack (s' : used) eVal
        t'      = readBack used (eval ctx t)
        in constr s' t' e'
    -- For neutrals, read-back the value, ignoring the type
    Neutral v _ -> readBack used v
    otherwise -> val

-- Normalization means first evaluating and then reading-back
normalize :: Ctx -> Term -> Term
normalize ctx e = readBack (keys ctx) (eval ctx e)

{- Linear head reduction -}

{-
headRedV :: Ctx -> Term -> Value
headRedV ctx (Var s) = getLeastEval ctx s
headRedV ctx (App k n) = doApply (headRedV ctx k) (eval ctx n)
headRedV ctx (Ind ty b s k) = doInd (eval ctx ty) (eval ctx b) (eval ctx s) (headRedV ctx k)
headRedV ctx (Fst t) = doFst (eval ctx t)
headRedV ctx (Snd t) = doSnd (eval ctx t)
headRedV ctx t = eval ctx t

--Gets the least evaluated form of 'x' from context
getLeastEval :: Ctx -> Ident -> Value
getLeastEval ctx s = case lookup s ctx of
    Just (Def _ e) -> eval ctx e
    Just (Decl ty) -> Neutral (Var s) (eval ctx ty)

--Do head reduction step
headRed :: Ctx -> Term -> Term
headRed ctx term = case term of
    Abst s t e -> Abst s t e'
        where e' = headRed (extend ctx s (Decl t)) e
    Sigma s t e -> Sigma s t e'
        where e' = headRed (extend ctx s (Decl t)) e
    Succ t     -> Succ (headRed ctx t)
    Pair t1 t2 -> Pair (headRed ctx t1) (headRed ctx t2)
    otherwise  -> readBack (keys ctx) (headRedV ctx term) 
-}

{- Printing utilities (should be in AbsCTT but these need 'readBack') -}

-- Print function for a term or value (which is read-back into
-- a term, except when debugging)
instance Show Term where
    show t = printTerm' 0 (if debug then t else readBack [] t)

-- Helper function; the first argument `i` measures the depth
-- of the term (but is reset in some cases), which is used to
-- avoid unnecessary parentheses
printTerm' :: Int -> Term -> String
printTerm' i = \case
    Var s        -> show s
    Universe     -> "U"
    TDef (s,t,e) t' ->
        "[" ++ show s ++ ":" ++ printTerm' 0 t ++ " = "
            ++ printTerm' 0 e ++ "]" ++ printTerm' 0 t'
    Abst s t e   -> par1 ++ abstS ++ par2
        where abstS = if s == Ident "" || not (containsVar s e)
                then -- A -> B (no dependency)
                    printTerm' (i+1) t ++ " -> " ++ printTerm' 0 e
                else
                    "[" ++ show s ++ ":" ++ printTerm' 0 t ++ "]" ++ printTerm' 0 e
    Sigma s t e  -> par1 ++ abstS ++ par2
        where abstS = if s == Ident "" || not (containsVar s e)
                then -- A * B (no dependency)
                    printTerm' (i+1) t ++ " * " ++ printTerm' 0 e
                else
                    "<" ++ show s ++ ":" ++ printTerm' 0 t ++ ">"
                        ++ printTerm' 0 e
    Pair t1 t2 -> par1 ++ printTerm' i t1 ++ "," ++ printTerm' i t2 ++ par2
    Fst t -> par1 ++ printTerm' (i + 1) t ++ ".1" ++ par2
    Snd t -> par1 ++ printTerm' (i + 1) t ++ ".2" ++ par2
    Sum ty1 ty2           -> par1 ++ printTerm' (i + 1) ty1 ++ " + "
        ++ printTerm' (i + 1) ty2 ++ par2
    InL t1                -> par1 ++ "inl " ++ printTerm' (i + 1) t1 ++ par2
    InR t2                -> par1 ++ "inr " ++ printTerm' (i + 1) t2 ++ par2
    Split ty f1 f2 x      -> par1 ++ "split " ++ printTerm' (i+1) ty ++ " "
         ++ printTerm' (i+1) f1 ++ " " ++ printTerm' (i+1) f2  ++ " "
            ++ printTerm' (i+1) x ++ par2
    App fun arg -> par1 ++ printTerm' (i+1) inner ++ " " ++ unwords printedArgs ++ par2
        where (inner,args) = collectApps (App fun arg) []
              printedArgs  = map (printTerm' (i+1)) args
    Nat             -> "N"
    Zero            -> "Z"
    Succ t          -> par1 ++ "S " ++ printTerm' (i+1) t ++ par2
    Ind ty b s n    -> par1 ++ "ind " ++ printTerm' (i+1) ty ++ " "
        ++ printTerm' (i+1) b ++ " " ++ printTerm' (i+1) s ++ " "
            ++ printTerm' (i+1) n ++ par2
    I               -> "I"
    I0              -> "0"
    I1              -> "1"
    Sys sys         -> showSystem sys
    Partial phi t   -> "[" ++ show phi ++ "]" ++ printTerm' (i+1) t
    -- If the restriction is empty, don't print it, otherwise it
    -- could be mistaken for a partial type with false formula
    Restr sys t     -> (if null sys then "" else showSystem sys) ++ printTerm' (i+1) t
    Comp fam phi i0 u b i' -> par1 ++ "comp " ++ printTerm' (i+1) fam
        ++ " (" ++ show phi ++ ") " ++ printTerm' (i+1) i0 ++ " "
            ++ printTerm' (i+1) u ++  " " ++ printTerm' (i+1) b
                ++  " " ++ printTerm' (i+1) i' ++ par2
    -------- Used only when debugging, to print proper values
    Closure cl ctx -> "Cl(" ++ show cl ++ ")"-- ";" ++ showCtx ctx ++ ")"
    Neutral v t  -> printTerm' i v -- "N{" ++ printTerm' i v ++ "}:" ++ printTerm' (i+1) t
    TermV v -> show v
    -- Parentheses are not needed if `i` is zero
    where (par1,par2) = if i == 0 then ("","") else ("(",")")

-- Print a context (in one line)
showCtx :: Ctx -> String
showCtx ctx = "[" ++ intercalate ", " (map showEntry (reverse ctx)) ++ "]"

-- Print a single context entry
showEntry :: (Ident,CtxEntry) -> String
showEntry (s,Decl ty) = show s ++ " : " ++ show ty
showEntry (s,Def ty val) = show s ++ " : " ++ show ty ++ " = " ++ show val
showEntry (s,Val val) = show s ++ " => " ++ show val

-- Print a system
showSystem :: System -> String
showSystem sys = "[" ++ sysS ++ "]"
    where sysS = intercalate " | " $
            map (\(ff,t) -> show ff ++ " -> " ++ show t) sys