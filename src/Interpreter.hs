module Interpreter(interpret, deInterp, fullInterpreter, runInterpreter) where
    
import Environment
import AST
import Defaults
import Parser

import Control.Monad.Except
import Control.Monad.Reader
import Text.Parsec

import qualified Data.Map as M

data SomeError = ParseError ParseError | RuntimeError InterpreterError
    deriving Show

runInterpreter :: String -> [Object] -> IO (Either SomeError [Object])
runInterpreter s v = fmap snd <$> fullInterpreter s M.empty v

fullInterpreter :: String -> M.Map String Object -> [Object] -> IO (Either SomeError (M.Map String Object, [Object]))
fullInterpreter input initialE initalS = case parseN input of
    Left err -> return . Left $ ParseError err
    Right ast -> do 
        v <- runExceptT $ runReaderT (deInterp (mapM_ interpret ast) initialE initalS ast) input
        case v of
            Left err -> return . Left $ RuntimeError err
            Right val -> return . Right $ (contents val, result val)

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
    push (head x)
interpret ImmediateSelf = do
    nesting <- pop
    values <- ask
    case nesting of
        Number n
            | n >= 0 && n < toInteger (length values) -> push (values !! fromInteger n) >> interpret Execute
        o -> throwError . IndexingWithNonNumberError $ o
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
        Environment.Code c e' -> local (code:) . saveAndRestoreEnvironment $ do
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
