{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Eval where

import Data.List (intercalate,nub,find)
import Data.Foldable (foldrM)
import Data.Maybe (mapMaybe,fromJust)

import Ident
import Interval
import CoreCTT

import Debug.Trace

debug :: Bool
debug = False

myTrace :: String -> a -> a
myTrace s x = if debug then trace s x else x

lookupType :: Ident -> Ctx -> Value
lookupType s [] = error $ "[lookupType] got unknown identifier " ++ show s
lookupType s ((s',entry):ctx) = if s == s' then
        case entry of
            Decl ty     -> eval ctx ty
            Def  ty def -> eval ctx ty
            Val _       -> lookupType s ctx
    else
        lookupType s ctx

eval :: Ctx -> Term -> Value
eval ctx t = myTrace ("[eval] " ++ show t ++ ", ctx = " ++ showCtx (filter (\(s,_) -> s `elem` (vars t)) ctx)) $ case t of
    Var s -> case lookup s ctx of
        Nothing -> error $ "[eval] not found var `" ++ show s ++ "` in ctx"
        Just (Val v)     -> v
        Just (Decl ty)   -> Neutral (Var s) (eval ctx ty)
        Just (Def ty e)  -> eval ctx e
    Universe           -> Universe
    Abst s ty e        -> Closure t ctx
    App e1 e2          -> doApply (eval ctx e1) (eval ctx e2)
    Sigma s ty e       -> Closure t ctx
    Pair t1 t2         -> Pair (eval ctx t1) (eval ctx t2)
    Fst t              -> doFst (eval ctx t)
    Snd t              -> doSnd (eval ctx t)
    Sum ty1 ty2        -> Sum (eval ctx ty1) (eval ctx ty2)
    InL t1             -> InL (eval ctx t1)
    InR t2             -> InR (eval ctx t2)
    Split x ty f1 f2   -> doSplit (eval ctx x) (eval ctx ty) (eval ctx f1) (eval ctx f2)
    Nat                -> Nat
    Zero               -> Zero
    Succ t             -> Succ (eval ctx t)
    Ind ty base step n -> doInd ty' base' step' n'
        where ty'   = eval ctx ty
              base' = eval ctx base
              step' = eval ctx step
              n'    = eval ctx n
    I  -> I
    I0 -> I0
    I1 -> I1
    Sys sys               -> simpl emptyDirEnv (Sys $ evalSystem ctx sys)
    Partial phi t         -> foldPartial (evalDisjFormula ctx phi) (eval ctx t)
    Restr sys t           -> foldRestr (evalSystem ctx sys) (eval ctx t)
    Comp fam phi i0 u b i -> {-doComp ctx (eval ctx fam) (evalDisjFormula ctx phi)
        (eval ctx i0) (eval ctx u) (eval ctx b) (eval ctx i)-}
        doComp ctx fam phi i0 u b i
    otherwise             -> error $ "[eval] got " ++ show t

evalConjFormula :: Ctx -> ConjFormula -> Maybe ConjFormula
evalConjFormula ctx conj = myTrace ("[evalConjFormula: " ++ show conj ++ "] " ++ "entries = " ++ show entries) conj'
    where
        --entries     = nub $ filter (\(s,v) -> s `elem` (vars conj)) (reverse $ getBindings ctx)
        entries'    = filter (\(s,v) -> s `elem` (vars conj)) (getBindings ctx)
        entries     = map (\s -> fromJust $ find (\(s',v) -> s' == s) entries') (nub $ map fst entries') --get only the last one
        renamings   = concatMap (\case (s,Neutral (Var s') I) -> [(s,s')]; _ -> []) entries
        renamedConj = foldr substConj conj renamings
        vals        = filter (\(s,v) -> v == I0 || v == I1) entries
        conj'       = foldrM evalConj renamedConj vals

evalConj :: (Ident,Value) -> ConjFormula -> Maybe ConjFormula
evalConj (s,I0) conj@(Conj cf) = myTrace ("[evalConj] conj' = " ++ show conj' ++ ", direnv = " ++ show (conjToDirEnv conj')) $
    if conjToDirEnv conj `makesTrueAtomic` Eq1 s
        || inconsistent (conjToDirEnv conj') then
        Nothing
    else
        Just conj'
    where
        conj' = Conj . nub $ concatMap (\case
            Eq0 s' | s == s' -> [];
            Diag s1 s2 -> if s == s1 && s1 == s2 then []
                else [if s == s1 then Eq0 s2
                    else if s == s2 then Eq0 s1 else Diag s1 s2];
            af -> [af]
            ) cf
evalConj (s,I1) conj@(Conj cf) = myTrace ("[evalConj] conj' = " ++ show conj' ++ ", direnv = " ++ show (conjToDirEnv conj')) $
    if conjToDirEnv conj `makesTrueAtomic` Eq0 s
        || inconsistent (conjToDirEnv conj') then
        Nothing
    else Just conj'
    where
        conj' = Conj . nub $ concatMap (\case
            Eq1 s' | s == s' -> [];
            Diag s1 s2 -> if s == s1 && s1 == s2 then []
               else [if s == s1 then Eq1 s2
                   else if s == s2 then Eq1 s1 else Diag s1 s2];
            af -> [af]) cf

evalDisjFormula :: Ctx -> DisjFormula -> DisjFormula
evalDisjFormula ctx (Disj df) = myTrace ("[evalDisjFormula: " ++ show (Disj df) ++ "] ==> " ++ show (Disj df')) $ if Conj [] `elem` df' then
        fTrue
    else
        Disj df'
    where df' = mapMaybe (evalConjFormula ctx) df

foldRestr :: System -> Value -> Value
foldRestr sys v = uncurry Restr $ foldRestr' sys v
    where
        foldRestr' :: System -> Value -> (System,Value)
        foldRestr' sys v = case v of
            Restr sys' v' -> foldRestr' (sys ++ sys') v'
            otherwise     -> (sys,v)

foldPartial :: DisjFormula -> Value -> Value
foldPartial disj v = uncurry Partial $ foldPartial' disj v
    where
        foldPartial' :: DisjFormula -> Value -> (DisjFormula,Value)
        foldPartial' disj v = case v of
            Partial disj' v' -> foldPartial' (dnf disj' disj) v'
            otherwise        -> (disj,v)
        dnf :: DisjFormula -> DisjFormula -> DisjFormula
        dnf (Disj df1) (Disj df2) = Disj $
            [cf1 `meet` cf2 | cf1 <- df1, cf2 <- df2]  

evalSystem :: Ctx -> System -> System
evalSystem ctx sys = concatMap (\(phi,t) -> evalConjFormula' phi (eval ctx t)) sys
    where
        evalConjFormula' phi v = case evalConjFormula ctx phi of
            Nothing -> []
            Just cf -> [(cf,v)]


extract :: Value -> (Ident -> Term -> Term -> Value,Ident,Term,Term)
extract (Abst  s t e) = (Abst,s,t,e)
extract (Sigma s t e) = (Sigma,s,t,e)
extract v             = error $ "[extract] got " ++ show v

evalClosure :: Value -> Value -> Value
evalClosure (Closure (Abst  s t e) ctx) arg  = eval (if s == Ident "" then ctx else extend ctx s (Val arg)) e
evalClosure (Closure (Sigma s t e) ctx) arg = eval (if s == Ident "" then ctx else extend ctx s (Val arg)) e
evalClosure v arg = error $ "[evalClosure] got non-closure " ++ show v 

doApply :: Value -> Value -> Value
doApply fun@(Closure (Abst s t e) ctx) arg = {-myTrace ("[doApply] fun = " ++ show fun ++ ", arg = " ++ show arg) $-} 
    evalClosure fun arg
doApply (Restr _ fun) arg = doApply fun arg
doApply (Neutral f fty) arg = let aty = doApply fty arg in
    case aty of
        Restr sys v | any (\(cf,_) -> emptyDirEnv `makesTrueConj` cf) sys ->
            snd . fromJust $ find (\(cf,_) -> emptyDirEnv `makesTrueConj` cf) sys
        otherwise -> Neutral (App (Neutral f fty) arg) aty
doApply v arg = error $ "[doApply] got " ++ show v ++ ", " ++ show arg

-- Evaluates nat-induction
doInd :: Value -> Value -> Value -> Value -> Value
doInd ty base step n = case n of
    Zero     -> myTrace ("[doInd] base case") $ base
    Succ n'  -> myTrace ("[doInd] succ case") $ doApply fun prev
        where
            fun = doApply step n'
            prev = doInd ty base step n'
    Neutral n Nat -> myTrace ("[doInd] neutral case: n = " ++ show n ++ ", ty = " ++ show ty) $
        Neutral (Ind ty base step n) (doApply ty (Neutral n Nat)) --neutral value
    otherwise -> error $ "[doInd] got " ++ show n

doFst :: Value -> Value
doFst v = case v of
    Pair v1 v2   -> v1
    Neutral x (Closure (Sigma s t e) ctx) -> Neutral (Fst v) (eval ctx t)
    Neutral x (Restr _ cl) -> doFst (Neutral x cl)
    otherwise -> error $ "[doFst] got " ++ show v

doSnd :: Value -> Value
doSnd v = case v of
    Pair v1 v2   -> v2
    Neutral x ty@(Closure (Sigma s t e) ctx) -> Neutral (Snd v) (evalClosure ty x)
    Neutral x (Restr _ cl) -> doSnd (Neutral x cl)
    otherwise -> error $ "[doSnd] got " ++ show v

doSplit :: Value -> Value -> Value -> Value -> Value
doSplit x ty f1 f2 = case x of
    InL x1 -> doApply f1 x1
    InR x2 -> doApply f2 x2
    Neutral x' ty' -> Neutral (Split x ty f1 f2) (doApply ty x)

doComp :: Ctx -> Term -> DisjFormula -> Term -> Term -> Term -> Term -> Value
doComp ctx fam phi i0 u b i = 
    let famV = eval ctx fam
        phiV = evalDisjFormula ctx phi
        i0V  = eval ctx i0
        uV   = eval ctx u
        bV   = eval ctx b
        iV   = eval ctx i
    in if emptyDirEnv `makesTrueDisj` phiV then
        doApply uV iV
    else if conv (keys ctx) emptyDirEnv i0V iV then
        bV
    else --myTrace ("[doComp] " ++ show (Comp fam phi i0 u b i)) $
        let var  = newVar (keys ctx) (Ident "_i")
            var2 = case uV of Closure (Abst v _ _) _ -> newVar (keys ctx) v
        in case doApply famV (Neutral (Var var) I) of
            cl@(Closure (Abst x ty e) ctx') | eval ctx' ty /= I -> myTrace ("[doComp-∏] " ++ show (Comp fam phi i0 u b i) ++ "\n\tty1 = " ++ show ty1{- ++ "\n\ttyc = " ++ show tyc ++ "\n\tty = " ++ show ty-}) $
                Closure (Abst u1 ty comp) ctx
                where
                    u1   = newVar (var : var2 : keys ctx) (Ident "u")
                    ut1  = Comp (Abst var I ty1) fFalse i (Abst (Ident "") I (Sys [])) (Var u1) i0
                    utt  = Comp (Abst var I ty1) fFalse i (Abst (Ident "") I (Sys [])) (Var u1) (Var var)
                    comp = Comp (Abst var I tyc) phi i0 u' (App b ut1) i
                    ty1  = readBack (keys ctx) $ eval ctx' ty
                    tyc  = readBack (keys ctx) $ evalClosure cl (eval ctx utt)
                    ty   = readBack (keys ctx) $ case doApply famV iV of Closure (Abst x ty e) ctx' -> eval ctx' ty
                    u' = case eqFalse phiV of
                        True  -> u
                        False -> Abst var2 I (App (App u (Var var2)) utt)
            {-cl@(Closure (Abst x ty e) ctx') | eval ctx' ty == I -> myTrace ("[doComp-∏_I] " ++ show (Comp fam phi i0 u b i) ++ "\n\tty1 = " ++ show ty1{- ++ "\n\ttyc = " ++ show tyc ++ "\n\tty = " ++ show ty-}) $
                Closure (Abst j I comp) ctx
                where
                    j = newVar (var : var2 : keys ctx) x
                    comp = Comp (Abst var I ty1) phi i0 u' (App b (Var j)) i
                    ty1 = readBack (keys ctx) $ evalClosure cl (Neutral (Var j) I)
                    u' = case eqFalse phiV of
                        True  -> u
                        False -> Abst var2 I $ Sys $ map (\conj -> (conj,App u i)) df : [(Eq0 j, )]-}
            cl@(Closure (Sigma x ty e) ctx') -> myTrace ("[doComp-∑] " ++ show (Comp fam phi i0 u b i)) $ Pair c0 c1
                where
                    c0  = doComp ctx (Abst var I ty1) phi i0 u0 (Fst b) i
                    c1  = doComp ctx (Abst var I ty2) phi i0 u1 (Snd b) i
                    ty1 = readBack (keys ctx) $ eval ctx' ty
                    --c0' = Comp (Abst var I ty1) phi i0 u0 (Fst b) (Var var)
                    c0' = doComp ctx (Abst var I ty1) phi i0 u0 (Fst b) (Var var)
                    ty2 = readBack (keys ctx) $ evalClosure cl c0' -- (eval ctx c0')
                    (u0,u1) = case eqFalse phiV of
                        True  -> (u,u)
                        False -> (Abst var2 I (Fst (App u (Var var2))),Abst var2 I (Snd (App u (Var var2))))
            otherwise -> myTrace ("[doComp-neutral]") $
                Neutral (Comp famV phiV i0V uV bV iV) (eval ctx compty)
                where
                    sys    = getCompSys phi i0 u b i
                    compty = Restr sys (App fam i)

getNeutralType :: Value -> Value
getNeutralType (Neutral _ ty) = ty
getNeutralType v = error $ "[getNeutralType] got non-neutral term " ++ show v

isSimplRestr :: DirEnv -> Value -> Bool
isSimplRestr dirs ty = case ty of
    Restr sys _ -> isSimplSys dirs sys
    otherwise   -> False

simplifyRestr :: DirEnv -> Value -> Value
simplifyRestr dirs (Restr sys _) = simplifySys dirs sys
simplifyRestr _ v = error $ "[simplifyValue] got " ++ show v

isSimplSys :: DirEnv -> System -> Bool
isSimplSys dirs sys = any ((dirs `makesTrueConj`) . fst) sys

simplifySys :: DirEnv -> System -> Value
simplifySys dirs sys = snd . fromJust $
    find ((dirs `makesTrueConj`) . fst) sys

simpl :: DirEnv -> Value -> Value
simpl dirs (Sys sys) | isSimplSys dirs sys = simplifySys dirs sys
simpl dirs v = v

readBack :: [Ident] -> Value -> Term
readBack used v = case v of
    App fun arg -> App (readBack used fun) (readBack used arg)
    Succ v -> Succ (readBack used v)
    Fst v -> Fst (readBack used v)
    Snd v -> Snd (readBack used v)
    Pair v1 v2 -> Pair (readBack used v1) (readBack used v2)
    Sum v1 v2 -> Sum (readBack used v1) (readBack used v2)
    InL v -> InL (readBack used v)
    InR v -> InR (readBack used v)
    Split x ty f1 f2 -> Split (readBack used x) (readBack used ty) (readBack used f1) (readBack used f2)
    Sys sys -> Sys (mapElems (readBack used) sys)
    Partial phi ty -> foldPartial phi (readBack used ty)
    Restr sys ty -> foldRestr (mapElems (readBack used) sys) (readBack used ty)
    Ind ty b e n -> Ind (readBack used ty) (readBack used b) (readBack used e) (readBack used n)
    Comp fam phi i0 u b i -> Comp (readBack used fam) phi (readBack used i0)
        (readBack used u) (readBack used b) (readBack used i)
    fun@(Closure cl ctx) -> let
        (constr,s,t,e) = extract cl
        used'   = used ++ keys ctx
        s'      = newVar used' s
        t'      = readBack used (eval ctx t)
        fun'    = Closure (constr s t e) (extend ctx s' (Decl t))
        eVal    = evalClosure fun' (Neutral (Var s') (eval ctx t))
        e'      = readBack (s' : used') eVal
        in constr s' t' e'
    Neutral v _ -> readBack used v
    otherwise -> v


normalize :: Ctx -> Term -> Term
normalize ctx e = readBack (keys ctx) (eval ctx e)

{- Linear head reduction -}

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
headRed ctx t = case t of
    Abst s t e -> Abst s t e'
        where e' = headRed (extend ctx s (Decl t)) e
    Sigma s t e -> Sigma s t e'
        where e' = headRed (extend ctx s (Decl t)) e
    Succ t     -> Succ (headRed ctx t)
    Pair t1 t2 -> Pair (headRed ctx t1) (headRed ctx t2)
    otherwise  -> readBack (keys ctx) (headRedV ctx t) 


{- Printing utilities (should be in AbsCTT but these need 'readBack') -}

instance Show Term where
    show t = printTerm' 0 (if debug then t else readBack [] t)

printTerm' :: Int -> Term -> String
printTerm' i t = case t of
    Var s        -> show s
    Universe     -> "U"
    Abst s t e   -> par1 ++ abstS ++ par2
        where abstS = if not (containsVar s e)
                then -- A -> B
                    printTerm' (i+1) t ++ " -> " ++ printTerm' 0 e
                else
                    "[" ++ show s ++ ":" ++ printTerm' 0 t ++ "]" ++ printTerm' 0 e
    Sigma s t e  -> par1 ++ abstS ++ par2
        where abstS = if not (containsVar s e)
                then -- A * B
                    printTerm' (i+1) t ++ " * " ++ printTerm' 0 e
                else
                    "<" ++ show s ++ ":" ++ printTerm' 0 t ++ ">" ++ printTerm' 0 e
    Pair t1 t2 -> par1 ++ printTerm' i t1 ++ "," ++ printTerm' i t2 ++ par2
    Fst t -> par1 ++ printTerm' (i + 1) t ++ ".1" ++ par2
    Snd t -> par1 ++ printTerm' (i + 1) t ++ ".2" ++ par2
    Sum ty1 ty2           -> par1 ++ printTerm' (i + 1) ty1 ++ " + " ++ printTerm' (i + 1) ty2 ++ par2
    InL t1                -> par1 ++ "inl " ++ printTerm' (i + 1) t1 ++ par2
    InR t2                -> par1 ++ "inr " ++ printTerm' (i + 1) t2 ++ par2
    Split x ty f1 f2      -> par1 ++ "split " ++ printTerm' (i+1) x ++ " " ++ printTerm' (i+1) ty ++ " "
         ++ printTerm' (i+1) f1 ++ " " ++ printTerm' (i+1) f2 ++ par2
    App fun arg -> par1 ++ printTerm' (i+1) inner ++ " " ++ unwords printedArgs ++ par2
        where (inner,args) = collectApps (App fun arg) []
              printedArgs  = map (printTerm' (i+1)) args
    Nat             -> "N"
    Zero            -> "Z"
    Succ t          -> par1 ++ "S " ++ printTerm' (i+1) t ++ par2 --if isNum then show (n + 1) else "S" ++ printTerm' (i+1) t
        where (isNum,n) = isNumeral t
    Ind ty b s n    -> par1 ++ "ind-N " ++ printTerm' (i+1) ty ++ " " ++ printTerm' (i+1) b ++ " "
         ++ printTerm' (i+1) s ++ " " ++ printTerm' (i+1) n ++ par2
    I               -> "I"
    I0              -> "0"
    I1              -> "1"
    Sys sys         -> showSystem sys
    Partial phi t   -> "[" ++ show phi ++ "]" ++ printTerm' (i+1) t
    Restr sys t     -> showSystem sys ++ printTerm' (i+1) t
    Comp fam phi i0 u b i' -> par1 ++ "comp " ++ printTerm' (i+1) fam ++ " (" ++ show phi ++ ") " ++
        printTerm' (i+1) i0 ++ " " ++ printTerm' (i+1) u ++  " " ++ printTerm' (i+1) b ++  " " ++ printTerm' (i+1) i' ++ par2
    --------
    Closure cl ctx  -> "Cl(" ++ show cl ++ {-"," ++ showCtx ctx ++-} ")"
    Neutral v t  -> printTerm' i v -- "N{" ++ printTerm' i v ++ "}:" ++ printTerm' (i+1) t
    where (par1,par2) = if i == 0 then ("","") else ("(",")")

{-
isVal :: CtxEntry -> Bool
isVal (Val _) = True
isVal _       = False

extend :: Ctx -> Ident -> CtxEntry -> Ctx
extend ctx s e | (isVal e) = case lookup s ctx of
    Just (Val _) -> myTrace ("[###############] duplicated " ++ showEntry (s,e) ++ " in ctx = " ++ showCtx ctx) $ if s == Ident "" then ctx else (s,e) : ctx
    otherwise    -> if s == Ident "" then ctx else (s,e) : ctx
extend ctx s e = if s == Ident "" then ctx else (s,e) : ctx
-}

showCtx :: Ctx -> String
showCtx ctx = "[" ++ intercalate ", " (map showEntry (reverse ctx)) ++ "]"

showOnlyShort :: String -> String
showOnlyShort s = if length s < 0 then "..." else s --debug

showEntry :: (Ident,CtxEntry) -> String
showEntry (s,Decl ty) = show s ++ " : " ++ showOnlyShort (show ty)
showEntry (s,Def ty val) = show s ++ " : " ++ showOnlyShort (show ty) ++ " = " ++ showOnlyShort (show val)
showEntry (s,Val val) = show s ++ " => " ++ show val

showSystem :: System -> String
showSystem sys = "[" ++ intercalate " | " (map (\(ff,t) -> show ff ++ " -> " ++ show t) sys) ++ "]"


------------------



getCompSys :: DisjFormula -> Term -> Term -> Term -> Term -> System
getCompSys (Disj df) i0 u b i = eq ++ map (\conj -> (conj,App u i)) df
    where
        i'  = case i  of { Neutral (Var x) _ -> Var x ; _ -> i  }
        i0' = case i0 of { Neutral (Var x) _ -> Var x ; _ -> i0 }
        eq  = case (i0',i') of
            (I0,I0)       -> [(Conj [],b)]
            (I0,I1)       -> []
            (I0,Var s)    -> [(Conj [Eq0 s],b)]
            (Var s,I0)    -> [(Conj [Eq0 s],b)]
            (Var s,I1)    -> [(Conj [Eq1 s],b)]
            (Var s,Var s') -> [(Conj [Diag s s'],b)]
            (I1,I0)       -> []
            (I1,I1)       -> [(Conj [],b)]
            (I1,Var s)    -> [(Conj [Eq0 s],b)]
            _             -> error $ "[getCompSys] got " ++ show (i,i0)

class Convertible a where
    conv :: [Ident] -> DirEnv -> a -> a -> Bool

convPartialConj :: [Ident] -> ConjFormula -> DirEnv -> Value -> Value -> Bool
convPartialConj used conj dirs v1 v2 = myTrace ("[convPartialConj] conj = " ++ show conj ++ ", v1 = " ++ show v1 ++ ", v2 = " ++ show v2) $
    let dirs' = addConj dirs conj
    in inconsistent dirs' || conv used dirs' v1 v2

convPartialDisj :: [Ident] -> DisjFormula -> DirEnv -> Value -> Value -> Bool
convPartialDisj used (Disj df) dirs v1 v2 = myTrace ("[convPartialDisj] disj = " ++ show (Disj df) ++ ", v1 = " ++ show v1 ++ ", v2 = " ++ show v2) $
    all (\conj -> convPartialConj used conj dirs v1 v2) df

sameKind :: Term -> Term -> Bool
sameKind (Abst {}) (Abst {}) = True
sameKind (Sigma {}) (Sigma {}) = True
sameKind _ _ = False

proofIrrelevant :: DirEnv -> Value -> Bool
proofIrrelevant dirs ty = case ty of
    Restr sys ty' -> any ((dirs `makesTrueConj`) . fst) sys || proofIrrelevant dirs ty'
    Closure cl ctx -> case cl of
        Abst s t e  -> proofIrrelevant dirs (eval (extend ctx s (Val . Var $ newVar (keys ctx) s)) e)
        Sigma s t e -> proofIrrelevant dirs (eval ctx t) &&
            proofIrrelevant dirs (eval (extend ctx s (Val . Var $ newVar (keys ctx) s)) e)
    otherwise -> False

instance Convertible Value where
    conv used dirs v1 v2 = myTrace ("[conv] " ++ show v1 ++ " ~ " ++ show v2 ++ ", dirs = " ++ show dirs)
        v1 == v2 || let cnv = conv used dirs in case (v1,v2) of
            (Universe,Universe) -> True
            (Closure cl1 ctx1,Closure cl2 ctx2) | sameKind cl1 cl2 -> let
                (_,s1,t1,e1) = extract cl1
                (_,s2,t2,e2) = extract cl2
                var = newVar (used ++ keys ctx2) s1
                t1V = eval ctx1 t1
                t2V = eval ctx2 t2
                e1' = evalClosure v1 (Neutral (Var var) t1V)
                e2' = evalClosure v2 (Neutral (Var var) t2V)
                in cnv t1V t2V && conv (var : used) dirs e1' e2'
            (Closure (Abst s1 t1 e1) ctx1,v2@(Neutral _ (Closure (Abst _ _ _) _))) -> let
                var = newVar used s1
                t1V = eval ctx1 t1
                e1' = evalClosure v1 (Neutral (Var var) t1V)
                e2' = doApply (simpl dirs v2) (Neutral (Var var) t1V)
                in conv (var : used) dirs e1' e2'
            (v1@(Neutral _ (Closure (Abst _ _ _) _)),Closure (Abst s2 t2 e2) ctx2) -> let
                var = newVar used s2
                t2V = eval ctx2 t2
                e2' = evalClosure v2 (Neutral (Var var) t2V)
                e1' = doApply (simpl dirs v1) (Neutral (Var var) t2V)
                in conv (var : used) dirs e1' e2'
            {- Sigma types -}
            (Fst v1,Fst v2) -> cnv v1 v2
            (Snd v1,Snd v2) -> cnv v1 v2
            (Pair v1 v1',Pair v2 v2') -> cnv v1 v2 &&
                cnv v1' v2'
            (v,Pair v1 v2) -> cnv (doFst $ simpl dirs v) v1 &&
                cnv (doSnd $ simpl dirs v) v2
            (Pair v1 v2,v) -> cnv v1 (doFst $ simpl dirs v) &&
                cnv v2 (doSnd $ simpl dirs v)
            {- Naturals -}
            (Nat,Nat)           -> True
            (Zero,Zero)         -> True
            (Succ n1,Succ n2)   -> cnv n1 n2
            {- Cubical -}
            (I,I)               -> True
            (I0,I0)             -> True
            (I1,I1)             -> True
            (Sys sys1,v2) | isSimplSys dirs sys1 ->
                cnv (simpl dirs v1) v2
            (v1,Sys sys2) | isSimplSys dirs sys2 ->
                cnv v1 (simpl dirs v2)
            (Sys sys1,Sys sys2) -> conv used dirs sys1 sys2
            {-(Comp fam1 phi1 _ u1 b1 i1,v2) | dirs `makesTrueDisj` phi1 -> --TODO.. not needed!?
                cnv (doApply u1 i1) v2
            (v1,Comp fam2 phi2 _ u2 b2 i2) | dirs `makesTrueDisj` phi2 ->
                cnv v1 (doApply u2 i2)-}
            (Partial phi1 v1,Partial phi2 v2) -> eqFormulas dirs phi1 phi2 &&
                cnv v1 v2
            -- (Restr sys1 t1,Restr sys2 t2) | /= -> conv used dirs sys1 sys2 &&
            --     cnv t1 t2
            (Restr sys1 t1,Restr sys2 t2) -> cnv t1 t2 &&
                convPartialDisj used (getSystemFormula sys1) dirs (Sys sys1) (Sys sys2) &&
                convPartialDisj used (getSystemFormula sys2) dirs (Sys sys2) (Sys sys1)
            {- Neutrals -}
            (Var s1,Var s2) -> s1 == s2
            (App f1 a1,App f2 a2) -> cnv f1 f2 && cnv a1 a2 
            (Ind ty1 b1 s1 n1,Ind ty2 b2 s2 n2) ->
                cnv ty1 ty2 && cnv b1 b2 &&
                cnv s1  s2  && cnv n1 n2
            (Comp fam1 phi1 i01 u1 b1 i1,Comp fam2 phi2 i02 u2 b2 i2) ->
                cnv fam1 fam2 && eqFormulas dirs phi1 phi2 &&
                cnv i01 i02 && cnv u1 u2 && cnv b1 b2 && cnv i1 i2
            (Neutral _ ty1,v2) | isSimplRestr dirs ty1 ->
                    cnv (simplifyRestr dirs ty1) v2
            (v1,Neutral _ ty2) | isSimplRestr dirs ty2 ->
                    cnv v1 (simplifyRestr dirs ty2)
            (Neutral (Var x1) I,Neutral (Var x2) I) -> 
                dirs `makesTrueAtomic` (Diag x1 x2)
            (Neutral (Var x1) I,I0) -> 
                dirs `makesTrueAtomic` (Eq0 x1)
            (Neutral (Var x1) I,I1) -> 
                dirs `makesTrueAtomic` (Eq1 x1)
            (I0,Neutral (Var x2) I) -> 
                dirs `makesTrueAtomic` (Eq0 x2)
            (I1,Neutral (Var x2) I) -> 
                dirs `makesTrueAtomic` (Eq1 x2)
            (Neutral v1 ty1,Neutral v2 ty2) -> 
                proofIrrelevant dirs ty1 || cnv v1 v2
            otherwise -> False

instance Convertible System where
    conv used dirs sys1 sys2 =
        eqFormulas dirs (getSystemFormula sys1) (getSystemFormula sys2) &&
        all (\(conj,t1,t2) -> convPartialConj used conj dirs t1 t2) meets
            where meets = [(conj1 `meet` conj2, sys1 `at` conj1, sys2 `at` conj2) |
                        conj1 <- keys sys1, conj2 <- keys sys2]

