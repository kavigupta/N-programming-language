module Interpreter(interpret, deInterp, runInterpreter) where
    
import Environment
import AST
import Defaults
import Parser

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Text.Parsec

import qualified Data.List.Safe as S

import qualified Data.Map as M

data SomeError = ParseError ParseError | RuntimeError InterpreterError
    deriving Show

runInterpreter :: String -> Either SomeError Stack
runInterpreter input = case parseN input of
    Left err -> Left $ ParseError err
    Right ast -> case deInterp (mapM_ interpret ast) (Child M.empty Defaults, []) of
        Left err -> Left $ RuntimeError err
        Right val -> Right $ snd val

interpret :: AST -> InterpAct ()
interpret (Symbol c) = do
    x <- lookupE [c]
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
    (_, s) <- get
    case index of
        Number n -> case s S.!! n of
            Nothing -> throwError StackUnderflow
            Just x -> push x
        o -> throwError $ IndexingWithNonNumberError o
interpret Lookup = do
    name <- pop
    case name of
        Str s -> do
            val <- lookupE s
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

    
lookupE :: String -> InterpAct Object
lookupE s = do
    (frames, _) <- get
    lookupIn frames s

lookupIn :: Environment -> String -> InterpAct Object
lookupIn frames s = case frames of
    Defaults -> indexBuiltinFunction s
    (Child f fs) -> case s `M.lookup` f of
        Nothing -> lookupIn fs s
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

-- pop :: Object -> State FullEnv ()