module Interpreter(interpret, deInterp, runInterpreter) where
    
import Environment
import AST
import Defaults
import Parser

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Text.Parsec

import qualified Data.Map as M

data SomeError = ParseError ParseError | RuntimeError InterpreterError
    deriving Show

runInterpreter :: String -> Either SomeError FullEnv
runInterpreter input = case parseN input of
    Left err -> Left $ ParseError err
    Right ast -> case deInterp (mapM_ interpret ast) startingEnv of
        Left err -> Left $ RuntimeError err
        Right val -> Right val

interpret :: AST -> InterpAct ()
interpret (Symbol c) = do
    x <- lookupE True [c]
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
            val <- lookupE False s
            push val
        o -> throwError $ LookingUpNonStringError o
interpret Duplicate = do
    x <- pop
    push x
    push x
interpret Execute = do
    code <- pop
    (e, s) <- get
    case code of
        Environment.Code c e' -> local (const code) $ do
            put (e', s)
            mapM_ interpret c
            (_, s') <- get
            put (e, s')
        Environment.PrimitiveFunction _ v -> v
            
        o -> throwError $ ExecutedNonCodeError o
interpret (AST.Code c) = do
    (e, _) <- get
    push (Environment.Code c e)
interpret (LString s) = push (Str s)
interpret (LNumber n) = push (Number n)
interpret (NewFrame x) = do
    (e, s) <- get
    put (Child M.empty e, s)
    interpret x
    (Child _ e', s') <- get
    put (e', s')

    
lookupE :: Bool -> String -> InterpAct Object
lookupE implicitLiteral s = do
    (frames, _) <- get
    lookupIn implicitLiteral frames s

lookupIn :: Bool -> Environment -> String -> InterpAct Object
lookupIn implicitLiteral frames s = case frames of
    Defaults -> indexBuiltinFunction implicitLiteral s
    (Child f fs) -> case s `M.lookup` f of
        Nothing -> lookupIn implicitLiteral fs s
        (Just x) -> return x

(=:=) :: Object -> Object -> InterpAct ()
(Str var) =:= val   = do
    (frames, s) <- get
    case frames of
        Defaults -> throwError $ CriticalError "Tried to assign in the global frame"
        (Child f fs)
            | var `M.member` f  -> throwError $ MultipleAssignmentError var
            | otherwise         -> put (Child (M.insert var val f) fs, s)
var =:= _  = throwError $ BindingToNonStringError var
