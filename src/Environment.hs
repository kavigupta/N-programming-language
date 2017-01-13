{-# LANGUAGE DoAndIfThenElse, FlexibleContexts #-}
module Environment (
    Object(..),
    FullEnv,
    InterpreterError(..),
    InterpAct,
    getEnv, setEnv,
    push, pop, indexStack,
    newFrame, lookupE, (=:=),
    objEqual,
    deInterp,
    saveAndRestoreEnvironment,
    result, contents
) where

import AST(AST, printCode)
import RegFile

import Prelude hiding((!!), lookup)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map hiding (map)
import Data.List.Safe hiding (lookup, insert)
import qualified Data.List as L

import Text.Parsec

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
    show (Environment.Code c e) = code ++ ":" ++ show e
        where
        code = "[" ++ L.intercalate "," (map printCode c) ++ "]"
    show (Pair car cdr) = "(" ++ withNoParens car cdr ++ ")"
    show (PrimitiveFunction name _) = "#" ++ name

objEqual :: Object -> Object -> Bool
objEqual (Number x) (Number y)  = x == y
objEqual (Number _) _           = False
objEqual (Str x) (Str y)        = x == y
objEqual (Str _) _              = False
objEqual Nil Nil                = True
objEqual Nil _                  = False
objEqual (Pair a b) (Pair c d)  = objEqual a c && objEqual b d
objEqual (Pair _ _) _           = False
objEqual (Code _ _) (Code _ _)  = False
objEqual (Code _ _) _           = False
objEqual (PrimitiveFunction x _) (PrimitiveFunction y _) = x == y
objEqual (PrimitiveFunction _ _) _ = False

withNoParens :: Object -> Object -> String
withNoParens car Nil = show car
withNoParens car (Pair cadr cddr) = show car ++ " " ++ withNoParens cadr cddr
withNoParens car cdr = show car ++ " . " ++ show cdr

data Environment = Environment {mappings :: Map String Object}

instance Show Environment where
    show (Environment m) = "{" ++ intercalate ", " (item <$> toList m) ++ "}"
        where
        item (x, y) = x ++ "=" ++ show y

newtype Stack = Stack [Object]
    deriving (Show)

data FullEnv = FullEnv {environment :: Environment, stack :: Stack}
    deriving Show

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
    | LibraryError ParseError
        deriving Show

type InterpAct x = ReaderT [Object] (StateT (FullEnv, RegFile Object) (ReaderT String (ExceptT InterpreterError IO))) x

deInterp :: InterpAct () -> Map String Object -> [Object] -> [AST] -> ReaderT String (ExceptT InterpreterError IO) FullEnv
deInterp x items initial ast = withRegs
    where
    withRegs = fst . snd <$> runStateT withAST (FullEnv (Environment items) (Stack initial), initialRegFile)
    withAST = runReaderT x [Code ast newFrame]

push :: (MonadState (FullEnv, a) m) => Object -> m ()
push x = do
    (Stack s) <- getStack
    setStack . Stack $ x : s

pop :: (MonadState (FullEnv, a) m, MonadError InterpreterError m) => m Object
pop = do
    s <- getStack
    case s of
        Stack [] -> throwError StackUnderflow
        Stack (o:os) -> do
            setStack $ Stack os
            return o

indexStack :: Integer -> InterpAct Object
indexStack n = do
    (Stack s) <- getStack
    case s !! n of
        Nothing -> throwError StackUnderflow
        Just x -> return x

type Defaults = Bool -> String -> InterpAct Object

lookupE :: Defaults -> Bool -> String -> InterpAct Object
lookupE indexBuiltinFunction implicitLiteral s = do
    (FullEnv frames _) <- getFEnv
    lookupIn indexBuiltinFunction implicitLiteral frames s

lookupIn :: Defaults -> Bool -> Environment -> String -> InterpAct Object
lookupIn indexBuiltinFunction implicitLiteral f s = case s `lookup` mappings f of
        Nothing -> indexBuiltinFunction implicitLiteral s
        (Just x) -> return x

(=:=) :: Object -> Object -> InterpAct ()
(Str var) =:= val   = do
    (FullEnv (Environment f) s) <- getFEnv

    if var `member` f then
        throwError $ MultipleAssignmentError var
    else
        putFEnv $ FullEnv (Environment $ insert var val f) s
var =:= _  = throwError $ BindingToNonStringError var

newFrame :: Environment
newFrame = Environment empty

getStack :: (MonadState (FullEnv, a) m) => m Stack
getStack = stack <$> getFEnv

setStack :: (MonadState (FullEnv, a) m) => Stack -> m ()
setStack s = do
    e <- getEnv
    putFEnv (FullEnv e s)

getEnv :: (MonadState (FullEnv, a) m) => m Environment
getEnv = environment <$> getFEnv

setEnv :: (MonadState (FullEnv, a) m) => Environment -> m ()
setEnv e = do
    s <- getStack
    putFEnv (FullEnv e s)

saveAndRestoreEnvironment :: InterpAct () -> InterpAct ()
saveAndRestoreEnvironment act = do
    (FullEnv e _) <- getFEnv
    act
    setEnv e

getFEnv :: (MonadState (FullEnv, a) m) => m FullEnv
getFEnv = fst <$> get

putFEnv :: (MonadState (FullEnv, a) m) => FullEnv -> m ()
putFEnv e = do
    (_, b) <- get
    put (e, b)

result :: FullEnv -> [Object]
result FullEnv {stack=Stack l} = l

contents :: FullEnv -> Map String Object
contents FullEnv {environment=Environment e} = e
