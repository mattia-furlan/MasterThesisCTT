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
debug = True

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
eval :: Ctx -> Term -> Value --eval ctx term = myTrace ("[eval] " ++ show term ++ ", ctx = " ++ showCtx (filter (\(s,_) -> s `elem` (vars term)) ctx)) $ case term of
eval ctx term = case term of
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
    Sys sys               -> simpl emptyDirEnv (Sys $ evalSystem ctx sys)
    Partial phi t         -> foldPartial (evalDisjFormula ctx phi) (eval ctx t)
    Restr sys t           -> foldRestr (evalSystem ctx sys) (eval ctx t)
    Comp fam phi i0 u b i -> doComp ctx fam phi i0 u b i
    otherwise             -> error $ "[eval] got " ++ show term

-- Evaluate a conjunctive formula
evalConjFormula :: Ctx -> ConjFormula -> Maybe ConjFormula
evalConjFormula ctx conj = myTrace ("[evalConjFormula: " ++ show conj ++ "] " ++ " => " ++ show conj') conj'
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
evalConj (s,I0) conj@(Conj cf) = myTrace ("[evalConj] conj' = " ++ show conj' ++ ", direnv = " ++ show (conjToDirEnv conj')) $
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
evalConj (s,I1) conj@(Conj cf) = myTrace ("[evalConj] conj' = " ++ show conj' ++ ", direnv = " ++ show (conjToDirEnv conj')) $
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

-- Helper function, used below to delete duplicates
-- in restriction and partial types
nubFst :: Eq a => ([a],b) -> ([a],b)
nubFst (as,b) = (nub as,b)

-- Simplify cascading restriction types into one restriction type
-- E.g. [psi_1 -> t_1]([psi_2 -> t_2]([psi_3 -> t_3]A)) becomes
-- [psi_1 -> t_1 | psi_2 -> t_2 | psi_3 -> t_3]A
-- I call `nubFst` to delete duplicate formulas
foldRestr :: System -> Value -> Value
foldRestr sys0 v0 = uncurry Restr . nubFst $ foldRestr' sys0 v0
    where
        foldRestr' :: System -> Value -> (System,Value)
        foldRestr' sys v = case v of
            Restr sys' v' -> foldRestr' (sys ++ sys') v'
            otherwise     -> (sys,v)

-- Simplify cascading partial types into one partial type
-- E.g. [phi_1]([phi_2]([phi_3]A)) becomes
-- [phi /\ psi_2 /\ psi_3]A
-- I call `nubFst` to delete duplicate formulas
foldPartial :: DisjFormula -> Value -> Value
foldPartial (Disj df0) v0 = (\(df,v) -> Partial (Disj df) v) .
    nubFst $ foldPartial' df0 v0
    where
        foldPartial' :: [ConjFormula] -> Value -> ([ConjFormula],Value)
        foldPartial' df v = case v of
            Partial (Disj df') v' -> foldPartial' (dnf df' df) v'
            otherwise             -> (df,v)
        -- Get the conjuction of the two disjunctive formulas,
        -- in disjunctive normal form
        dnf :: [ConjFormula] -> [ConjFormula] -> [ConjFormula]
        dnf df1 df2 = [cf1 `meet` cf2 | cf1 <- df1, cf2 <- df2] 

-- Evaluate a system
evalSystem :: Ctx -> System -> System
evalSystem ctx sys =
    concatMap (\(phi,t) -> evalConjFormula' phi (eval ctx t)) sys
    where
        -- Need to handle the case of false formulas
        evalConjFormula' phi v = case evalConjFormula ctx phi of
            Nothing -> []
            Just cf -> [(cf,v)]

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
evalClosure (Closure (Abst  s _ e) ctx) arg =
    eval (if s == Ident "" then ctx else extend ctx s (Val arg)) e
evalClosure (Closure (Sigma s _ e) ctx) arg =
    eval (if s == Ident "" then ctx else extend ctx s (Val arg)) e
evalClosure v arg = error $ "[evalClosure] got non-closure " ++ show v
    ++ " applied to " ++ show arg

-- Handler of `App` (function application, i.e. ∏-type eliminator)
doApply :: Value -> Value -> Value
-- Standard case: do β-reduction
doApply fun@(Closure Abst{} _) arg = evalClosure fun arg
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
        where sys' = mapSys (doApply fam) sys
    -- System case
    Sys sys -> Sys $ mapSys (doInd fam base step) sys
    -- Partial type case
    Neutral n (Partial phi Nat) -> 
        Neutral (Ind fam base step n)
            (foldPartial phi (doApply fam (Neutral n Nat)))
    otherwise -> error $ "[doInd] got " ++ show m

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
        where sys' = mapSys (doApply fam) sys
    -- System case
    Sys sys -> Sys $ mapSys (doSplit fam f1 f2) sys
    -- Partial type case
    Neutral _ (Partial phi (Sum ty1 ty2)) ->
        Neutral (Split fam f1 f2 (Neutral x (Sum ty1 ty2)))
            (foldPartial phi (doApply fam x))
    otherwise -> error $ "[doSplit] got " ++ show x

-- Utility function to handle eventually empty strings
ifEmpty :: Ident -> String -> Ident
ifEmpty (Ident "") s = Ident s
ifEmpty i          _ = i

-- Handler of composition (which is not an eliminator!)
doComp :: Ctx -> Term -> DisjFormula -> Term -> Term -> Term -> Term -> Value
--doComp ctx fam phi i0 u b i = myTrace ("[doComp-?] " ++ show (Comp fam phi i0 u b i) ++ ", " ++ show (Comp famV phiV i0V uV bV iV)) $
doComp ctx fam phi i0 u b i = 
    if isTrue phiV then  -- 1° trivial case: `phi` is True
        doApply uV iV
    else if conv (keys ctx) emptyDirEnv i0V iV then -- 2° trivial case: i = i_0
        bV
    else
        -- Fresh variables: `var` to pattern-match the type family
        --                  `var2` to handle the partially defined `u` 
        let var  = newVar (keys ctx) (Ident "_i")
            var2 = case uV of
                Closure (Abst v _ _) _ -> newVar (keys ctx) (ifEmpty v "j")
            emptySys = Abst (Ident "") I (Sys [])
        -- Evaluate the type-family, pattern-matching inside the closure
        in case doApply famV (Neutral (Var var) I) of
            -- ∏-type `[x:ty]e`, with `ty` a type
            cl@(Closure (Abst x ty _) ctx') | eval ctx' ty /= I -> myTrace ("[doComp-∏] " ++ show (Comp fam phi i0 u b i)) $
                Closure (Abst yi tyc comp) ctx
                where
                    -- Variable of the target type `tyc`, i.e. `ty(i)`
                    yi   = newVar (var : var2 : keys ctx) (ifEmpty x "u")
                    -- Transport of that variable at `i0` and `var'
                    yt0  = Comp (Abst var I ty1) fFalse i emptySys (Var yi) i0
                    yt   = Comp (Abst var I ty1) fFalse i emptySys (Var yi) (Var var)
                    -- Resulting composition
                    comp = Comp (Abst var I e') phi i0 u' (App b yt0) i
                    ty1  = readBack (keys ctx) $ eval ctx' ty
                    e'   = readBack (keys ctx) $ evalClosure cl (eval ctx yt)
                    -- Target type: `ty(i)`
                    tyc  = readBack (keys ctx) $ case doApply famV iV of
                        Closure (Abst _ ty' _) ctx'' -> eval ctx'' ty'
                    -- Apply functions to `yt` inside `u`
                    u' = case isFalse phiV of
                        True  -> u
                        False -> Abst var2 I (App (App u (Var var2)) yt)
            -- ∏-type `[x:I]e`     
            cl@(Closure (Abst x ty _) ctx') | eval ctx' ty == I -> myTrace ("[doComp-∏I] " ++ show (Comp fam phi i0 u b i)) $
                Closure (Abst x' I comp) ctx
                where
                    -- Fresh nterval variable
                    x'   = newVar (var : var2 : keys ctx) x
                    -- Resulting composition
                    comp = Comp (Abst var I e') phi i0 u' (App b (Var x')) i
                    e'   = readBack (keys ctx) $ evalClosure cl (Neutral (Var x') I)
                    -- Apply functions to `x'` inside `u`
                    u'   = case isFalse phiV of
                        True  -> u
                        False -> Abst var2 I (App (App u (Var var2)) (Var x'))
            -- ∑-type `<x:ty>e`
            cl@(Closure (Sigma _ ty _) ctx') -> myTrace ("[doComp-∑] " ++ show (Comp fam phi i0 u b i)) $ Pair c1 c2
                where
                    -- Composition on each pair
                    c1  = doComp ctx (Abst var I ty1) phi i0 u0 (Fst b) i
                    c2  = doComp ctx (Abst var I ty2) phi i0 u1 (Snd b) i
                    -- The type family of `c2` needs the comp. on the first component
                    ty1 = readBack (keys ctx) $ eval ctx' ty
                    c1' = doComp ctx (Abst var I ty1) phi i0 u0 (Fst b) (Var var)
                    ty2 = readBack (keys ctx) $ evalClosure cl c1'
                    -- Apply projections inside `u`
                    (u0,u1) = case isFalse phiV of
                        True  -> (u,u)
                        False -> (Abst var2 I (Fst (App u (Var var2))),
                                  Abst var2 I (Snd (App u (Var var2))))
            -- Coproduct `ty1 + ty2`
            Sum ty1 ty2 -> myTrace ("[doComp-Sum] " ++ show (Comp fam phi i0 u b i)) $
                -- If `b` is neutral, the result is neutral
                case bV of
                    Neutral{} -> doNeutralComp
                    otherwise -> inj comp
                where (inj,bV',ty) = case bV of
                      -- Extract the injection, the inner value and the type
                        InL b1 -> (InL,b1,ty1)
                        InR b2 -> (InR,b2,ty2)
                      -- Resulting composition
                      comp = doComp ctx (Abst var I ty') phi i0 u' b' i
                      b' = readBack (keys ctx) bV'
                      ty'  = readBack (keys ctx) ty
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
            Nat -> myTrace ("[doComp-Nat] " ++ show (Comp fam phi i0 u b i)) $ case bV of
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
            -- Restriction type `[phi]ty`
            Restr sysR ty | var `notElem` concatMap vars (keys sys) ->
                    doComp ctx (Abst var I ty') formula i0 u' b i
                where
                    formula = Disj $ phi' ++ psis
                    phi' = case phi of Disj ff -> ff 
                    psis = keys sysR
                    ty'  = readBack (keys ctx) ty
                    u'   = Abst var2 I (Sys sys')
                    -- Concatenate the two systems
                    sys' = map (\conj -> (conj,App u (Var var2))) phi'
                        ++ case doApply famV (Neutral (Var var2) I) of
                            Restr sys'' _ ->
                                map (\(conj,v) -> (conj,readBack (keys ctx) v)) sys''
            -- Neutral type family; the result of the composition is neutral too
            otherwise -> myTrace ("[doComp-neutral] " ++
                show (Comp fam phi i0 u b i)) $ doNeutralComp
    where
        -- Values computed for each argument (remember that Haskell is lazy!)
        famV = eval ctx fam
        phiV = evalDisjFormula ctx phi
        i0V  = eval ctx i0
        uV   = eval ctx u
        bV   = eval ctx b
        iV   = eval ctx i
        -- Compute the type of the composition, and prepare the neutral value
        doNeutralComp = simplNeutralValue $
            Neutral (Comp famV phiV i0V uV bV iV) (eval ctx compTy)
        sys    = getCompSys phi i0 u b i
        compTy = Restr sys (App fam i)

-- Simplify a neutral value if the type is a restriction type with a true
-- formula, otherwise do nothing. It is similar to `simplRestr`, used only
-- for `conv`, except that here we do not use the directions environment.
-- `simplNeutralValue` is used only in evaluation.
simplNeutralValue :: Value -> Value
simplNeutralValue neu@(Neutral _ ty) = case ty of
    Restr sys _ | isSimplSys emptyDirEnv sys ->
        simplSys emptyDirEnv sys
    otherwise -> neu

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
        where abstS = if not (containsVar s e)
                then -- A -> B (no dependency)
                    printTerm' (i+1) t ++ " -> " ++ printTerm' 0 e
                else
                    "[" ++ show s ++ ":" ++ printTerm' 0 t ++ "]" ++ printTerm' 0 e
    Sigma s t e  -> par1 ++ abstS ++ par2
        where abstS = if not (containsVar s e)
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
    Ind ty b s n    -> par1 ++ "ind-N " ++ printTerm' (i+1) ty ++ " "
        ++ printTerm' (i+1) b ++ " " ++ printTerm' (i+1) s ++ " "
            ++ printTerm' (i+1) n ++ par2
    I               -> "I"
    I0              -> "0"
    I1              -> "1"
    Sys sys         -> showSystem sys
    Partial phi t   -> "[" ++ show phi ++ "]" ++ printTerm' (i+1) t
    Restr sys t     -> showSystem sys ++ printTerm' (i+1) t
    Comp fam phi i0 u b i' -> par1 ++ "comp " ++ printTerm' (i+1) fam
        ++ " (" ++ show phi ++ ") " ++ printTerm' (i+1) i0 ++ " "
            ++ printTerm' (i+1) u ++  " " ++ printTerm' (i+1) b
                ++  " " ++ printTerm' (i+1) i' ++ par2
    -------- Used only when debugging, to print proper values
    Closure cl _ -> "Cl(" ++ show cl ++  ")"
    Neutral v t  -> "N{" ++ printTerm' i v ++ "}:" ++ printTerm' (i+1) t
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

-- Simplify a system if possibile, otherwise do nothing
simpl :: DirEnv -> Value -> Value
simpl dirs (Sys sys) | isSimplSys dirs sys = simplSys dirs sys
simpl dirs v = v

-- Check if the type can be simplified; in that case two values
-- of that type are automatically convertible, because they
-- shall reduce to the same value. That is, we don't need
-- to look inside the terms, i.e. we can ignore the proof
proofIrrelevant :: DirEnv -> Value -> Bool
proofIrrelevant dirs ty = case ty of
    Restr sys ty' -> isSimplRestr dirs ty
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
                e2' = doApply (simpl dirs v2) (Neutral (Var var) t1V)
                in conv (var : used) dirs e1' e2'
            -- η-rule for ∏ (second case)
            (Neutral _ (Closure Abst{} _),Closure (Abst s2 t2 _) ctx2) -> let
                var = newVar used s2
                t2V = eval ctx2 t2
                e2' = evalClosure v2 (Neutral (Var var) t2V)
                e1' = doApply (simpl dirs v1) (Neutral (Var var) t2V)
                in conv (var : used) dirs e1' e2'
            {- Sigma types -}
            (Fst v,Fst v') -> cnv v v'
            (Snd v,Snd v') -> cnv v v'
            (Pair vp1 vp1',Pair vp2 vp2') -> cnv vp1 vp2 &&
                cnv vp1' vp2'
            -- η-rule for ∑ (first case)
            (vp,Pair v v') -> cnv (doFst $ simpl dirs vp) v &&
                cnv (doSnd $ simpl dirs vp) v'
            -- η-rule for ∑ (second case)
            (Pair v v',vp) -> cnv v (doFst $ simpl dirs vp) &&
                cnv v' (doSnd $ simpl dirs vp)
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
                    cnv (simplRestr dirs ty1) v2
            (_,Neutral _ ty2) | isSimplRestr dirs ty2 ->
                    cnv v1 (simplRestr dirs ty2)
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

