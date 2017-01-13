{-# LANGUAGE DoAndIfThenElse, FlexibleContexts, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Environment (
    FObject,
    FullEnv,
    FInterpreterError,
    InterpAct(..),
    push, pop, indexStack,
    newFrame, lookupE, (=:=),
    objEqual,
    deInterp,
    localEnv,
    result, contents,
    close
) where

import AST(AST)
import RegFile
import Object
import Error

import Prelude hiding((!!), lookup)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map hiding (map)
import Data.List.Safe hiding (lookup, insert)

import Control.Lens

type FObject = Object Environment (InterpAct ())
type FInterpreterError = InterpreterError Environment (InterpAct ())

data Environment = Environment {_mappings :: Map String FObject}

instance Show Environment where
    show (Environment m) = "{" ++ intercalate ", " (item <$> toList m) ++ "}"
        where
        item (x, y) = x ++ "=" ++ show y

newtype Stack = Stack {_unStack :: [FObject]}
    deriving (Show)

data FullEnv = FullEnv {_environment :: Environment, _stack :: Stack}
    deriving Show


newtype InterpAct x = InterpAct {runInterpAct :: ReaderT [FObject] (StateT (FullEnv, RegFile FObject) (ReaderT String (ExceptT FInterpreterError IO))) x}
    deriving (Functor, Applicative, Monad, MonadIO, MonadError FInterpreterError, MonadReader [FObject], MonadState (FullEnv, RegFile FObject))

makeLenses ''Stack
makeLenses ''Environment
makeLenses ''FullEnv

deInterp :: InterpAct () -> Map String FObject -> [FObject] -> [AST] -> ReaderT String (ExceptT FInterpreterError IO) FullEnv
deInterp x items initial ast = withRegs
    where
    withRegs = fst . snd <$> runStateT withAST (FullEnv (Environment items) (Stack initial), initialRegFile)
    withAST = runReaderT (runInterpAct x) [Code ast newFrame]

push :: (MonadState (FullEnv, a) m) => FObject -> m ()
push x = _1 . stack . unStack %= (x:)

pop :: (MonadState (FullEnv, a) m, MonadError FInterpreterError m) => m FObject
pop = do
    s <- use $ _1 . stack . unStack
    case s of
        [] -> underflow
        (o:os) -> do
            _1 . stack . unStack .= os
            return o

indexStack :: Integer -> InterpAct FObject
indexStack n = do
    s <- use $ _1 . stack . unStack
    case s !! n of
        Nothing -> underflow
        Just x -> return x

type Defaults = Bool -> String -> InterpAct FObject

lookupE :: Defaults -> Bool -> String -> InterpAct FObject
lookupE indexBuiltinFunction implicitLiteral s = do
    f <- use $ _1 . environment . mappings
    case s `lookup` f of
        Nothing -> indexBuiltinFunction implicitLiteral s
        (Just x) -> return x

(=:=) :: FObject -> FObject -> InterpAct ()
(Str var) =:= val   = do
    f <- use $ _1. environment . mappings
    if var `member` f then
        multipleAssign var
    else
        _1 . environment .= (Environment $ insert var val f)
var =:= _  = lookupNonString var

newFrame :: Environment
newFrame = Environment empty

localEnv :: Environment -> InterpAct () -> InterpAct ()
localEnv env act = do
    e <- use $ _1 . environment
    _1 . environment .= env
    act
    _1 . environment .= e

close :: [AST] -> InterpAct FObject
close ast = Code ast <$> use (_1 . environment)

result :: FullEnv -> [FObject]
result e = e ^. stack . unStack

contents :: FullEnv -> Map String FObject
contents e = e ^. environment . mappings
