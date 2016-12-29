module Environment (
    Object(..),
    Environment(..),
    Frame, Stack, FullEnv,
    InterpreterError(..),
    Action, InterpAct,
    push, pop, deInterp, interp, objEqual
) where

import AST(AST, printCode)

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map hiding (map)
import qualified Data.List as L

data Object =
      Number Integer
    | Nil
    | Str String
    | Code [AST] Environment
    | Pair Object Object
    | PrimitiveFunction String (InterpAct ())

instance Show Object where
    show (Number x) = show x
    show Nil = "()"
    show (Str s) = show s
    show (Environment.Code e _) = "`" ++ L.intercalate "," (map printCode e) ++ "'"
    show (Pair car cdr) = "(" ++ withNoParens car cdr ++ ")"
    show (PrimitiveFunction name _) = "#" ++ name

objEqual :: Object -> Object -> InterpAct Bool
objEqual (Number x) (Number y)  = return $ x == y
objEqual (Number _) _           = return False
objEqual (Str x) (Str y)        = return $ x == y
objEqual (Str _) _              = return False
objEqual Nil Nil                = return True
objEqual Nil _                  = return False
objEqual (Pair a b) (Pair c d)  = (&&) <$> objEqual a c <*> objEqual b d
objEqual (Pair _ _) _           = return False
objEqual (Code _ _) (Code _ _)  = throwError CannotCompareCodeError
objEqual (Code _ _) _           = return False
objEqual (PrimitiveFunction x _) (PrimitiveFunction y _) = return $ x == y
objEqual (PrimitiveFunction _ _) _ = return False

withNoParens :: Object -> Object -> String
withNoParens car Nil = show car
withNoParens car (Pair cadr cddr) = show car ++ " " ++ withNoParens cadr cddr
withNoParens car cdr = show car ++ " . " ++ show cdr


data Environment = Defaults | Child Frame Environment

type Frame = Map String Object

type Stack = [Object]

type FullEnv = (Environment, Stack)

data InterpreterError
    = UnboundVariable String
    | CriticalError String
    | StackUnderflow
    | MultipleAssignmentError String
    | BindingToNonStringError Object
    | LookingUpNonStringError Object
    | IndexingWithNonNumberError Object
    | ExecutedNonCodeError Object
    | BuiltinTypeError String
    | CannotCompareCodeError
        deriving Show

type Action = FullEnv -> Either InterpreterError FullEnv

type InterpAct x = ReaderT Object (StateT FullEnv (Either InterpreterError)) x

deInterp :: InterpAct () -> Action
deInterp x (e, s) = snd <$> runStateT (runReaderT x (Code [] e)) (e, s)

interp :: Action -> InterpAct ()
interp act = do
    es <- get
    case act es of
        Left err -> throwError err
        Right x -> put x

push :: Object -> InterpAct ()
push x = do
    (e, s) <- get
    put (e, x:s)

pop :: InterpAct Object
pop = do
    (e, s) <- get
    case s of
        [] -> throwError StackUnderflow
        (o:os) -> do
            put (e, os)
            return o


