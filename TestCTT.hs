module Main where

import System.IO ( hFlush, stdout )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( return, when, forever, foldM, foldM_ )
import Control.Monad.State
import Data.List ( intercalate )
import Data.Maybe ( isJust )

import LexCTT   ( Token )
import ParCTT   ( pTerm, pToplevel, pProgram, myLexer )

import Ident
import Interval
import CoreCTT
import Eval
import TypeChecker

type Err = Either String

type ReplState = (Ctx,Term,[Ident]) -- Current context, last term checked, locked names

initReplState :: ReplState
initReplState = ([(Ident "I0",Def I I0),(Ident "I1",Def I I1)],Zero,[])

runFile :: FilePath -> StateT ReplState IO Bool
runFile f = do
    liftIO . putStrLn $ "Reading file " ++ f
    contents <- liftIO . readFile $ f
    res <- run contents
    liftIO $ when res $ putStrLn $ "\nFile " ++ f ++ " loaded successfully"
    return res

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

checkProgram :: Program -> StateT ReplState IO Bool
checkProgram (Program []) = return True
checkProgram (Program (toplevel : decls)) = do
    res <- checkSingleToplevel toplevel
    if res then
        checkProgram (Program decls)
    else
        return False

--Checks if a term contains undeclared variables (True = OK)
checkVars :: Ctx -> Term -> Bool
checkVars ctx t = case t of
    Var s _       -> isJust $ lookup s ctx
    Universe      -> True
    Abst s t e    -> checkVars ctx t && checkVars (extend ctx s (Decl {-dummy-}Universe)) e
    App fun arg   -> checkVars ctx fun && checkVars ctx arg
    Nat           -> True
    Zero          -> True
    Succ t        -> checkVars ctx t
    Ind ty b s n  -> checkVars ctx ty && checkVars ctx b && checkVars ctx s && checkVars ctx n
    I             -> True
    Sys sys       -> all (checkVars ctx) (elems sys)
    Partial phi t -> all (`elem` (keys ctx)) (vars phi) && checkVars ctx t
    Restr phi u t -> all (`elem` (keys ctx)) (vars phi) && checkVars ctx u && checkVars ctx t
    Comp psi x0 fam u -> checkVars ctx x0 && checkVars ctx fam && checkVars ctx u

checkSingleToplevel :: Toplevel -> StateT ReplState IO Bool
checkSingleToplevel (Example t) = do
    (ctx,_,_) <- get
    if not (checkTermShadowing [] t) then do
        liftIO . showErr $ "term '" ++ show t ++ "' contains shadowed variables"
        return False
    else if not (checkVars ctx t) then do
        liftIO . showErr $ "term '" ++ show t ++ "' contains undeclared variables"
        return False
    else
        checkSingleToplevel' (Example t)
checkSingleToplevel decl@(Declaration s t) = do
    (ctx,_,_) <- get
    if not (checkTermShadowing [] t) then do
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
checkSingleToplevel def@(Definition s t e) = do
    (ctx,_,_) <- get
    if not (checkTermShadowing [s] t && checkTermShadowing [s] e) then do --avoid "x : N->N = [x:N]x" 
        liftIO . showErr $ "definition of '" ++ show s ++ "' contains shadowed variables"
        return False
    else if not (checkVars ctx t && checkVars ctx e) then do
        liftIO . showErr $ "definition of '" ++ show s ++ "' contains undeclared variables"
        return False
    else case lookup s ctx of
        Nothing -> checkSingleToplevel' def
        Just _  -> do
            liftIO . showErr $ "context already contains name '" ++ show s ++ "'"
            return False

checkSingleToplevel' :: Toplevel -> StateT ReplState IO Bool
checkSingleToplevel' (Example t) = do
    (unlockedCtx,_,lockedNames) <- get
    let ctx = getLockedCtx lockedNames unlockedCtx
    let ty = inferType ctx emptyDirEnv t
    case ty of
        Left err -> do
           liftIO $ showErr err
           return False
        Right tyVal -> do
            liftIO . putStrLn $ "\n'" ++ show t ++ "' has (inferred) type '" ++ show tyVal ++ "'"
            let norm = normalize ctx t --since 't' typechecks, 't' must have a normal form
            liftIO . putStrLn $ "'" ++ show t ++ "' reduces to '" ++ show norm ++ "'"
            --let val = eval (ctx,ctxToEnv ctx) t --TODO
            --liftIO . putStrLn $ "'" ++ show t ++ "' evaluates to " ++ show val
            
            put (ctx,t,lockedNames)
            return True
checkSingleToplevel' (Declaration s t) = do
    (unlockedCtx,ans,lockedNames) <- get
    let ctx = getLockedCtx lockedNames unlockedCtx
    liftIO . putStrLn $ "\nType-checking term '" ++ show s ++ "' of type '" ++ show t  ++ "'"
    case addDecl ctx (s,t) of
        Left err -> do
            liftIO . showErr $ err
            return False
        Right ctx' -> do
            liftIO . putStrLn $ "Declaration check OK!"
            put (ctx',ans,lockedNames)
            return True
checkSingleToplevel' (Definition s t e) = do
    (unlockedCtx,ans,lockedNames) <- get
    let ctx = getLockedCtx lockedNames unlockedCtx
    liftIO . putStrLn $ "\nType-checking term '" ++ show s ++ "' of type '" ++ show t  ++ 
        "' and body '" ++ show e ++ "'"
    case addDef ctx (s,t,e) of
        Left err -> do
            liftIO . showErr $ err
            return False
        Right ctx' -> do
            liftIO . putStrLn $ "Type check OK!"
            liftIO . putStrLn $ replicate 50 '\n'
            put (ctx',ans,lockedNames)
            return True


doRepl :: StateT ReplState IO ()
doRepl = do
    (ctx,ans,lockedNames) <- get
    printIO "\n> "
    s <- liftIO getLine
    let w = words s
    case w of
        [":q"] -> do
            liftIO exitSuccess
        [":ans"] -> do
            liftIO . putStrLn $ show ans
        [":ctx"] -> do
            liftIO . printCtxLn $ ctx
        [":head"] -> do
            let ans' = headRed ctx ans
            liftIO . putStrLn $ show ans'
            put (ctx,ans',lockedNames)
        ":head" : sterm -> do
            case pTerm (myLexer (intercalate " " sterm)) of
                Left err ->
                    liftIO . putStrLn $ "could not parse term"
                Right term -> do
                    let ans' = headRed ctx term
                    liftIO . putStrLn $ show ans'
                    put (ctx,ans',lockedNames)
        ":clear" : idents -> do
            let ctx' = foldl removeFromCtx ctx (map Ident idents)
            put (ctx',ans,lockedNames)
        ":lock" : idents -> do
            let idents' = map Ident idents
                isInCtx = (`elem` (keys ctx))
                identsToAdd = filter isInCtx idents'
                identsWrong = filter (not . isInCtx) idents'
            when (length identsWrong > 0) $
                liftIO . putStrLn $ "identifier(s) " ++ intercalate ", " (map show identsWrong) ++
                    " not found in the current context" 
            let lockedNames' = identsToAdd ++ lockedNames
            put (ctx,ans,lockedNames')
        ":unlock" : idents -> do
            let lockedNames' = filter (not . (`elem` (map Ident idents))) lockedNames
            put (ctx,ans,lockedNames')
        ":unlockall" : idents ->
            put (ctx,ans,[])
        [":printlock"] ->
            liftIO . putStrLn $ "Locked names are: " ++ intercalate ", " (map show lockedNames)
        [":help"] -> do
            liftIO printUsage
        otherwise -> do
            let ts = myLexer s
            case pToplevel ts of
                Left err -> do
                    liftIO . putStrLn $ "\nParse failed!"
                    liftIO . showErr $ err
                Right toplevel -> do
                    --checkSingleToplevel (transformToplevel (keys ctx) toplevel)
                    checkSingleToplevel toplevel
                    return ()
    doRepl --loop

-- Adds a definition to the current context 
addDef :: Ctx -> (Ident,Term,Term) -> Either ErrorString Ctx
addDef ctx (s,t,e) = do
    checkType ctx emptyDirEnv t Universe -- Is 't' really a type?
    let tVal = eval ctx emptyDirEnv t
    checkType ctx emptyDirEnv e tVal -- Has 'e' type 't'?
    Right $ extend ctx s (Def t e)

-- Adds a definition to the current context 
addDecl :: Ctx -> (Ident,Term) -> Either ErrorString Ctx
addDecl ctx (s,t) = do
    checkType ctx emptyDirEnv t Universe -- Is 't' really a type?
    Right $ extend ctx s (Decl t)

printCtxLn :: Ctx -> IO ()
printCtxLn ctx = mapM_ (putStrLn . showEntry) (reverse ctx)

showErr :: String -> IO ()
showErr err = putStrLn $ "\nError: " ++ err

main :: IO ()
main = do
    printUsage
    args <- getArgs
    case args of
        [] -> do
            evalStateT doRepl initReplState
            exitSuccess
        fs -> evalStateT (
            do
                res <- foldM (\b fp -> (b &&) <$> runFile fp) True fs
                liftIO $ unless res $ exitSuccess
                (ctx,_,_) <- get
                liftIO . putStrLn $ "\nCurrent context is:"
                liftIO . printCtxLn $ ctx
                doRepl
            ) initReplState

printUsage :: IO ()
printUsage = do
    putStr $ unlines
        [ " -------------------------------------------------------------------------- "
        , "| Usage: ./TestCTT <file> .. <file>    load and type-check files           |"
        , "|                                      then start a REPL                   |"
        , "| Commands:                                                                |"
        , "|   x : <term> = <term>   add definition of 'x' to the context             |"
        , "|   <term>                infer type of t and normalize it                 |"
        , "|   :help                 print help                                       |"
        , "|   :q                    quit                                             |"
        , "|   :ans                  print the last term used                         |"
        , "|   :head                 apply head reduction to the last term used       |"
        , "|   :head <term>          apply head reduction to <term> (NOT type-checked)|"
        , "|   :ctx                  print current context                            |"
        , "|   :clear <id> .. <id>   remove <id>'s from context (recursively)         |"
        , "|   :lock <id> .. <id>    lock <id>'s definition                           |"
        , "|   :unlock <id> .. <id>  unlock <id>'s definition                         |"
        , "|   :unlockall            unlock every currently locked definition         |"
        , "|   :printlock            print locked definitions                         |"
        , " -------------------------------------------------------------------------- "
        ]
    hFlush stdout

printIO :: String -> StateT ReplState IO ()
printIO s = liftIO $ do
    putStr s
    hFlush stdout
