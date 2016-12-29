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
    saveAndRestoreEnvironment
) where

import AST(AST, printCode)

import Prelude hiding((!!), lookup)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map hiding (map)
import Data.List.Safe hiding (lookup, insert)
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
    show (Environment.Code c e) = "[" ++ L.intercalate "," (map printCode c) ++ "]" ++ show e
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

data Environment = Environment {mappings :: Map String Object}
    deriving (Show)

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
        deriving Show

-- type Action = FullEnv -> Either InterpreterError FullEnv

type InterpAct x = ReaderT Object (StateT FullEnv (Either InterpreterError)) x

deInterp :: InterpAct () -> Either InterpreterError FullEnv
deInterp x = snd <$> runStateT (runReaderT x (Code [] newFrame)) startingEnv
-- 
-- interp :: Action -> InterpAct ()
-- interp act = do
--     es <- get
--     case act es of
--         Left err -> throwError err
--         Right x -> put x

startingEnv :: FullEnv
startingEnv = FullEnv newFrame (Stack [])

push :: Object -> InterpAct ()
push x = do
    (Stack s) <- getStack
    setStack . Stack $ x : s

pop :: InterpAct Object
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
    (FullEnv frames _) <- get
    lookupIn indexBuiltinFunction implicitLiteral frames s

lookupIn :: Defaults -> Bool -> Environment -> String -> InterpAct Object
lookupIn indexBuiltinFunction implicitLiteral f s = case s `lookup` mappings f of
        Nothing -> indexBuiltinFunction implicitLiteral s
        (Just x) -> return x

(=:=) :: Object -> Object -> InterpAct ()
(Str var) =:= val   = do
    (FullEnv (Environment f) s) <- get
    
    if var `member` f then
        throwError $ MultipleAssignmentError var
    else
        put $ FullEnv (Environment $ insert var val f) s
var =:= _  = throwError $ BindingToNonStringError var

newFrame :: Environment
newFrame = Environment empty

getStack :: InterpAct Stack
getStack = stack <$> get

setStack :: Stack -> InterpAct ()
setStack s = do
    e <- getEnv
    put (FullEnv e s)

getEnv :: InterpAct Environment
getEnv = environment <$> get

setEnv :: Environment -> InterpAct ()
setEnv e = do
    s <- getStack
    put (FullEnv e s)

saveAndRestoreEnvironment :: InterpAct () -> InterpAct ()
saveAndRestoreEnvironment act = do
    (FullEnv e _) <- get
    act
    setEnv e
