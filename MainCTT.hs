module Main where

import System.IO ( hFlush, stdout )
import System.Environment ( getArgs )
import System.Exit        ( exitSuccess )
import Control.Monad.State
import Data.List ( intercalate )
import Data.Maybe ( isJust )

import ParCTT   ( pTerm, pToplevel, pProgram, myLexer )

import Ident
import Interval
import CoreCTT
import Eval
import TypeChecker

type Err = Either String

-- Current context, last term checked, list of locked names
type ReplState = (Ctx,Term,[Ident])

-- Initial state
initReplState :: ReplState
initReplState = ([],Zero,[])

-- Read from a file and call `run`
runFile :: FilePath -> StateT ReplState IO Bool
runFile f = do
    printLnIO $ "Reading file " ++ f
    contents <- liftIO . readFile $ f
    res <- run contents
    liftIO . when res . putStrLn $ "\nFile " ++ f ++ " loaded successfully"
    return res

-- Parse and call `checkProgram`
run :: String -> StateT ReplState IO Bool
run s = case pProgram ts of
    Left err -> do
        liftIO $ putStrLn "\nParse failed!"
        liftIO $ showErr err
        return False
    Right program -> do
        checkProgram program
    where
        ts = myLexer s

-- Check a whole program, by type-checking every top-level
-- declaration
checkProgram :: Program -> StateT ReplState IO Bool
checkProgram (Program []) = return True
checkProgram (Program (toplevel : decls)) = do
    res <- checkSingleToplevel toplevel
    if res then
        checkProgram (Program decls)
    else
        return False

-- Check if a term contains undeclared variables (True = OK)
checkVars :: Ctx -> Term -> Bool
checkVars ctx term = case term of
    Var s                 -> isJust $ lookup s ctx
    Universe              -> True
    Abst s t e            -> checkVars ctx t &&
        checkVars (extend ctx s (Decl {-dummy-}Universe)) e
    TDef (s,t,e) t'       -> checkVars ctx t &&
        checkVars (extend ctx s (Decl {-dummy-}Universe)) e &&
        checkVars (extend ctx s (Def t e)) t'
    App fun arg           -> checkVars ctx fun && checkVars ctx arg 
    Sigma s t e           -> checkVars ctx t &&
        checkVars (extend ctx s (Decl {-dummy-}Universe)) e
    Pair t1 t2            -> checkVars ctx t1 && checkVars ctx t2
    Fst t                 -> checkVars ctx t
    Snd t                 -> checkVars ctx t
    Sum ty1 ty2           -> checkVars ctx ty1 && checkVars ctx ty2
    InL t1                -> checkVars ctx t1
    InR t2                -> checkVars ctx t2
    Split ty f1 f2 x      -> checkVars ctx ty && checkVars ctx f1 &&
        checkVars ctx f2 && checkVars ctx x 
    Nat                   -> True
    Zero                  -> True
    Succ t                -> checkVars ctx t
    Ind ty b s n          -> checkVars ctx ty && checkVars ctx b &&
        checkVars ctx s && checkVars ctx n
    I                     -> True
    I0                    -> True
    I1                    -> True
    Sys sys               -> all (\phi -> all (`elem` keys ctx) (vars phi))
        (keys sys) && all (checkVars ctx) (elems sys)
    Partial phi t         -> all (`elem` keys ctx) (vars phi) &&
        checkVars ctx t
    Restr sys t           -> checkVars ctx (Sys sys) && checkVars ctx t
    Comp fam phi i0 u b i -> checkVars ctx fam &&
        all (`elem` keys ctx) (vars phi) && checkVars ctx i0 &&
        checkVars ctx u && checkVars ctx b && checkVars ctx i

-- Check a single top-level declaration, calling `checkSingleToplevel'`
-- Here we mostly check variables
checkSingleToplevel :: Toplevel -> StateT ReplState IO Bool
-- Example: infer its type
checkSingleToplevel (Example t) = do
    (ctx,_,_) <- get
    if not (checkTermShadowing (keys ctx) t) then do
        liftIO . showErr $ "term '" ++ show t ++ "' contains shadowed variables"
        return False
    else if not (checkVars ctx t) then do
        liftIO . showErr $ "term '" ++ show t ++ "' contains undeclared variables"
        return False
    else
        checkSingleToplevel' (Example t)
-- Add a declaration to the context
checkSingleToplevel decl@(Declaration s t) = do
    (ctx,_,_) <- get
    if not (checkTermShadowing (keys ctx) t) then do
        liftIO . showErr $ "term '" ++ show t ++ "' contains shadowed variables"
        return False
    else if not (checkVars ctx t) then do
        liftIO . showErr $ "term '" ++ show t ++ "' contains undeclared variables"
        return False
    else case lookup s ctx of
        Nothing -> checkSingleToplevel' decl
        Just _  -> do
            liftIO . showErr $ "context already contains name '" ++ show s ++ "'"
            return False
-- Add a definition to the context
checkSingleToplevel def@(Definition s t e) = do
    (ctx,_,_) <- get
    if not (checkTermShadowing (s : keys ctx) t &&
       checkTermShadowing (s : keys ctx) e) then do --avoid "x : N->N = [x:N]x" 
        liftIO . showErr $ "definition of '" ++ show s
            ++ "' contains shadowed variables"
        return False
    else if not (checkVars ctx t && checkVars ctx e) then do
        liftIO . showErr $ "definition of '" ++ show s
            ++ "' contains undeclared variables"
        return False
    else case lookup s ctx of
        Nothing -> checkSingleToplevel' def
        Just _  -> do
            liftIO . showErr $ "context already contains name '"
                ++ show s ++ "'"
            return False


checkSingleToplevel' :: Toplevel -> StateT ReplState IO Bool
checkSingleToplevel' (Example t) = do
    -- Get the context with the locked names (i.e. erasing definitions
    -- of locked names)
    (unlockedCtx,_,lockedNames) <- get
    let ctx = getLockedCtx lockedNames unlockedCtx
        ty  = inferType ctx emptyDirEnv t
    case ty of
        Left err -> do
           liftIO $ showErr err
           return False
        Right tyVal -> do
            printLnIO $ "\n'" ++ show t ++ "' has (inferred) type '"
                ++ show (readBack (keys ctx) tyVal) ++ "'"
            -- Since `t` typechecks, `t` must have a normal form
            let norm = normalize ctx t
            printLnIO $ "'" ++ show t ++ "' reduces to '" ++ show norm ++ "'"
            -- Update `ans`
            put (ctx,t,lockedNames)
            return True

checkSingleToplevel' (Declaration s t) = do
    -- Get the context with the locked names (i.e. erasing definitions
    -- of locked names)
    (unlockedCtx,_,lockedNames) <- get
    let ctx = getLockedCtx lockedNames unlockedCtx
    printLnIO $ "\nType-checking term '" ++ show s ++ "' of type '"
        ++ show t  ++ "'"
    case addDecl ctx (s,t) of
        Left err -> do
            liftIO . showErr $ err
            return False
        Right ctx' -> do
            printLnIO "Declaration check OK!"
            -- Update `ans`
            put (ctx',t,lockedNames)
            return True

checkSingleToplevel' (Definition s t e) = do
    -- Get the context with the locked names (i.e. erasing definitions
    -- of locked names)
    (unlockedCtx,_,lockedNames) <- get
    let ctx = getLockedCtx lockedNames unlockedCtx
    printLnIO $ "\nType-checking term '" ++ show s ++ "' of type '"
        ++ show t  ++ "' and body '" ++ show e ++ "'"
    case addDef ctx (s,t,e) of
        Left err -> do
            liftIO . showErr $ err
            return False
        Right ctx' -> do
            printLnIO "Type check OK!"
            -- Update `ans`
            put (ctx',e,lockedNames)
            return True

-- Main REPL (infinite) loop
doRepl :: StateT ReplState IO ()
doRepl = do
    (ctx,ans,lockedNames) <- get
    printIO "\n> "
    s <- liftIO getLine
    let w = words s
    case w of
        -- Quit
        [":q"] -> do
            liftIO exitSuccess
        -- Print last type-checked term
        [":ans"] -> do
            printLnIO $ show ans
        -- Show context (with locked names)
        [":ctx"] -> do
            liftIO . printCtxLn $ getLockedCtx lockedNames ctx
        {-- TODO
        [":head"] -> do
            let ans' = headRed ctx ans
            printLnIO $ show ans'
            put (ctx,ans',lockedNames)
        ":head" : sterm -> do
            case pTerm (myLexer (unwords sterm)) of
                Left err ->
                    printLnIO $ "could not parse term: " ++ err
                Right term -> do
                    let ans' = headRed ctx term
                    printLnIO $ show ans'
                    put (ctx,ans',lockedNames)
        -}
        -- Delete from context the given names (and also the ones
        -- that depend on them)
        ":clear" : idents -> do
            let ctx' = foldl removeFromCtx ctx (map Ident idents)
            put (ctx',ans,lockedNames)
        -- Lock a list of identiers
        ":lock" : idents -> do
            let idents' = map Ident idents
                isInCtx = (`elem` (keys ctx))
                identsToAdd = filter isInCtx idents'
                identsWrong = filter (not . isInCtx) idents'
            when (length identsWrong > 0) $
                printLnIO $ "identifier(s) " ++ intercalate ", " (map show identsWrong)
                    ++ " not found in the current context" 
            let lockedNames' = identsToAdd ++ lockedNames
            put (ctx,ans,lockedNames')
        -- Unlock a list of identiers
        ":unlock" : idents -> do
            let lockedNames' = filter (`notElem` map Ident idents) lockedNames
            put (ctx,ans,lockedNames')
        -- Clear the list of locked identiers
        [":unlockall"] ->
            put (ctx,ans,[])
        -- Show the locked identifiers
        [":printlock"] ->
            printLnIO $ "Locked names are: " ++ intercalate ", " (map show lockedNames)
        -- Show help menu
        [":help"] -> do
            liftIO printUsage
        -- Unknown command
        (':' : _ ) : _ ->
            printLnIO "Command not found. Type :help"
        -- Otherwise, check a new declaration
        otherwise -> do
            let ts = myLexer s
            case pToplevel ts of
                Left err -> do
                    printLnIO "\nParse failed!"
                    liftIO . showErr $ err
                Right toplevel -> do
                    _ <- checkSingleToplevel toplevel
                    return ()
    doRepl -- Repeat

-- Add a definition to the current context 
addDef :: Ctx -> (Ident,Term,Term) -> Either ErrorString Ctx
addDef ctx (s,t,e) = do
    checkType ctx emptyDirEnv t Universe -- Check that `t` is a type
    let tVal = eval ctx t
    checkType ctx emptyDirEnv e tVal -- Check that `e` has type `t`
    Right $ extend ctx s (Def t e)

-- Add a definition to the current context 
addDecl :: Ctx -> (Ident,Term) -> Either ErrorString Ctx
addDecl ctx (s,t) = do
    -- Check that `t` is a type or the interval
    unless (t == I) $ checkType ctx emptyDirEnv t Universe
    Right $ extend ctx s (Decl t)

-- Lock each given identifier in the context,
-- i.e. erase its eventual definition from the context
getLockedCtx :: [Ident] -> Ctx -> Ctx
getLockedCtx idents ctx0 = foldr getLockedCtx' ctx0 idents
    where
        getLockedCtx' :: Ident -> Ctx -> Ctx
        getLockedCtx' s ((s',Def ty def) : ctx) =
            if s == s' then (s,Decl ty) : ctx
                       else (s',Def ty def) : getLockedCtx' s ctx
        getLockedCtx' s ((s',Decl ty) : ctx) =
            (s',Decl ty) : getLockedCtx' s ctx
        getLockedCtx' _ ctx = ctx

-- Print the context, line by line
printCtxLn :: Ctx -> IO ()
printCtxLn ctx = mapM_ (putStrLn . showEntry) (reverse ctx)

-- Print an error
showErr :: String -> IO ()
showErr err = putStrLn $ "\nError: " ++ err

main :: IO ()
main = do
    printUsage
    args <- getArgs
    case args of
        -- No files given: start the REPL loop
        [] -> do
            evalStateT doRepl initReplState
            exitSuccess
        -- Some files given: parse each file, then start the REPL loop
        fs -> evalStateT (
            do
                -- `b` is the result of the type-check of each file
                res <- foldM (\b fp -> (b &&) <$> runFile fp) True fs
                liftIO $ unless res exitSuccess
                (ctx,_,_) <- get
                printLnIO "\nCurrent context is:"
                liftIO . printCtxLn $ ctx
                doRepl
            ) initReplState

-- Print help menu
printUsage :: IO ()
printUsage = do
    putStr $ unlines
        [ " -------------------------------------------------------------------------- "
        , "| Usage: ./CTT <file> .. <file>    load and type-check files               |"
        , "|                                  then start a REPL                       |"
        , "| Commands:                                                                |"
        , "|   x : <term>            add declaration of 'x' to the context            |"
        , "|   x : <term> = <term>   add definition of 'x' to the context             |"
        , "|   <term>                infer type of t and normalize it                 |"
        , "|   :help                 print help                                       |"
        , "|   :q                    quit                                             |"
        , "|   :ans                  print the last term used                         |"
        -- , "|   :head                 apply head reduction to the last term used       |"
        -- , "|   :head <term>          apply head reduction to <term> (NOT type-checked)|"
        , "|   :ctx                  print current context                            |"
        , "|   :clear <id> .. <id>   remove <id>'s from context (recursively)         |"
        , "|   :lock <id> .. <id>    lock <id>'s definition                           |"
        , "|   :unlock <id> .. <id>  unlock <id>'s definition                         |"
        , "|   :unlockall            unlock every currently locked definition         |"
        , "|   :printlock            print locked definitions                         |"
        , " -------------------------------------------------------------------------- "
        ]
    hFlush stdout

-- Auxiliary printing functions
printIO :: String -> StateT ReplState IO ()
printIO s = liftIO $ do
    putStr s
    hFlush stdout

printLnIO :: String -> StateT ReplState IO ()
printLnIO s = printIO $ s ++ "\n"
