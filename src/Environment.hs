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

-- type Action = FullEnv -> Either InterpreterError FullEnv

type InterpAct x = ReaderT [Object] (StateT FullEnv (ReaderT String (ExceptT InterpreterError IO))) x

deInterp :: InterpAct () -> Map String Object -> [Object] -> [AST] -> ReaderT String (ExceptT InterpreterError IO) FullEnv
deInterp x items initial ast = snd <$> runStateT (runReaderT x [Code ast newFrame]) (FullEnv (Environment items) $ Stack initial)
-- 
-- interp :: Action -> InterpAct ()
-- interp act = do
--     es <- get
--     case act es of
--         Left err -> throwError err
--         Right x -> put x

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

result :: FullEnv -> [Object]
result FullEnv {stack=Stack l} = l

contents :: FullEnv -> Map String Object
contents FullEnv {environment=Environment e} = e
