{-# LANGUAGE DoAndIfThenElse, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Environment (
    FObject,
    FullEnv,
    InterpreterError(..),
    InterpAct(..),
    getEnv, setEnv,
    push, pop, indexStack,
    newFrame, lookupE, (=:=),
    objEqual,
    deInterp,
    saveAndRestoreEnvironment,
    result, contents
) where

import AST(AST)
import RegFile
import Object

import Prelude hiding((!!), lookup)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map hiding (map)
import Data.List.Safe hiding (lookup, insert)

import Text.Parsec

type FObject = Object Environment (InterpAct ())

data Environment = Environment {mappings :: Map String FObject}

instance Show Environment where
    show (Environment m) = "{" ++ intercalate ", " (item <$> toList m) ++ "}"
        where
        item (x, y) = x ++ "=" ++ show y

newtype Stack = Stack [FObject]
    deriving (Show)

data FullEnv = FullEnv {environment :: Environment, stack :: Stack}
    deriving Show

data InterpreterError
    = UnboundVariable String
    | CriticalError String
    | StackUnderflow
    | MultipleAssignmentError String
    | BindingToNonStringError FObject
    | LookingUpNonStringError FObject
    | IndexingWithNonNumberError FObject
    | ExecutedNonCodeError FObject
    | BuiltinTypeError String
    | CannotCompareCodeError
    | LibraryError ParseError
        deriving Show

newtype InterpAct x = InterpAct {runInterpAct :: ReaderT [FObject] (StateT (FullEnv, RegFile FObject) (ReaderT String (ExceptT InterpreterError IO))) x}
    deriving (Functor, Applicative, Monad, MonadIO, MonadError InterpreterError, MonadReader [FObject], MonadState (FullEnv, RegFile FObject))

deInterp :: InterpAct () -> Map String FObject -> [FObject] -> [AST] -> ReaderT String (ExceptT InterpreterError IO) FullEnv
deInterp x items initial ast = withRegs
    where
    withRegs = fst . snd <$> runStateT withAST (FullEnv (Environment items) (Stack initial), initialRegFile)
    withAST = runReaderT (runInterpAct x) [Code ast newFrame]

push :: (MonadState (FullEnv, a) m) => FObject -> m ()
push x = do
    (Stack s) <- getStack
    setStack . Stack $ x : s

pop :: (MonadState (FullEnv, a) m, MonadError InterpreterError m) => m FObject
pop = do
    s <- getStack
    case s of
        Stack [] -> throwError StackUnderflow
        Stack (o:os) -> do
            setStack $ Stack os
            return o

indexStack :: Integer -> InterpAct FObject
indexStack n = do
    (Stack s) <- getStack
    case s !! n of
        Nothing -> throwError StackUnderflow
        Just x -> return x

type Defaults = Bool -> String -> InterpAct FObject

lookupE :: Defaults -> Bool -> String -> InterpAct FObject
lookupE indexBuiltinFunction implicitLiteral s = do
    frames <- getEnv
    lookupIn indexBuiltinFunction implicitLiteral frames s

lookupIn :: Defaults -> Bool -> Environment -> String -> InterpAct FObject
lookupIn indexBuiltinFunction implicitLiteral f s = case s `lookup` mappings f of
        Nothing -> indexBuiltinFunction implicitLiteral s
        (Just x) -> return x

(=:=) :: FObject -> FObject -> InterpAct ()
(Str var) =:= val   = do
    (Environment f) <- getEnv

    if var `member` f then
        throwError $ MultipleAssignmentError var
    else
        setEnv (Environment $ insert var val f)
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
    e <- getEnv
    act
    setEnv e

getFEnv :: (MonadState (FullEnv, a) m) => m FullEnv
getFEnv = fst <$> get

putFEnv :: (MonadState (FullEnv, a) m) => FullEnv -> m ()
putFEnv e = do
    (_, b) <- get
    put (e, b)

result :: FullEnv -> [FObject]
result FullEnv {stack=Stack l} = l

contents :: FullEnv -> Map String FObject
contents FullEnv {environment=Environment e} = e
