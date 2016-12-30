module Interpreter(interpret, deInterp, debugInterpreter, runInterpreter) where
    
import Environment
import AST
import Defaults
import Parser

import Control.Monad.Except
import Control.Monad.Reader
import Text.Parsec

data SomeError = ParseError ParseError | RuntimeError InterpreterError
    deriving Show

runInterpreter :: String -> [Object] -> IO (Either SomeError [Object])
runInterpreter s v = fmap result <$> debugInterpreter s v

debugInterpreter :: String -> [Object] -> IO (Either SomeError FullEnv)
debugInterpreter input inital = case parseN input of
    Left err -> return . Left $ ParseError err
    Right ast -> do 
        v <- runExceptT $ runReaderT (deInterp (mapM_ interpret ast) inital) input
        case v of
            Left err -> return . Left $ RuntimeError err
            Right val -> return . Right $ val

interpret :: AST -> InterpAct ()
interpret (Symbol c) = do
    x <- lookupE indexBuiltinFunction True [c]
    push x
interpret Definition = do
    top <- pop
    next <- pop
    top =:= next
interpret Self = do
    x <- ask
    push x
interpret Index = do
    index <- pop
    case index of
        Number n -> indexStack n >>= push
        o -> throwError $ IndexingWithNonNumberError o
interpret Lookup = do
    name <- pop
    case name of
        Str s -> do
            val <- lookupE indexBuiltinFunction False s
            push val
        o -> throwError $ LookingUpNonStringError o
interpret Duplicate = do
    x <- pop
    push x
    push x
interpret Quine = do
    program <- lift ask
    push (Str program)
interpret Execute = do
    code <- pop
    case code of
        Environment.Code c e' -> local (const code) . saveAndRestoreEnvironment $ do
            setEnv e'
            mapM_ interpret c
        Environment.PrimitiveFunction _ v -> v
            
        o -> throwError $ ExecutedNonCodeError o
interpret (AST.Code c) = do
    e <- getEnv
    push (Environment.Code c e)
interpret (LString s) = push (Str s)
interpret (LNumber n) = push (Number n)
interpret (NewFrame x) = saveAndRestoreEnvironment $ do
    setEnv newFrame
    interpret x
