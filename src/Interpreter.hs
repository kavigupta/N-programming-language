module Interpreter(interpret, deInterp, fullInterpreter, runInterpreter) where

import Environment
import AST
import Defaults
import Parser
import RegFile
import Object
import Error

import Control.Monad.Except
import Control.Monad.Reader
import Text.Parsec

import qualified Data.Map as M

data SomeError = ParseError ParseError | RuntimeError FInterpreterError
    deriving Show

runInterpreter :: String -> [FObject] -> IO (Either SomeError [FObject])
runInterpreter s v = fmap snd <$> fullInterpreter s M.empty v

fullInterpreter :: String -> M.Map String FObject -> [FObject] -> IO (Either SomeError (M.Map String FObject, [FObject]))
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
interpret Swap = do
    x <- pop
    y <- pop
    push x
    push y
interpret Parents = do
    nesting <- pop
    values <- ask
    case nesting of
        Number n
            | n >= 0 && n < toInteger (length values) -> push (values !! fromInteger n) >> interpret Execute
        o -> indexWithNoNumber o
interpret Index = do
    index <- pop
    case index of
        Number n -> indexStack n >>= push
        o -> indexWithNoNumber o
interpret Lookup = do
    name <- pop
    case name of
        Str s -> do
            val <- lookupE indexBuiltinFunction False s
            push val
        o -> lookupNonString o
interpret Duplicate = do
    x <- pop
    push x
    push x
interpret Quine = do
    program <- InterpAct $ lift ask
    push (Str program)
interpret Execute = do
    code <- pop
    case code of
        Object.Code c e' -> local (code:) . localEnv e' $ mapM_ interpret c
        Object.PrimitiveFunction _ v -> v
        o -> executeNonCode o
interpret Conditional = do
        alternative <- pop
        consequent <- pop
        condition <- pop
        push $ if truthy condition then consequent else alternative
    where
    truthy :: FObject -> Bool
    truthy (Number x) = x /= 0
    truthy (Str s) = s /= ""
    truthy (Object.Code c _) = c /= []
    truthy Nil = False
    truthy (Pair _ _) = True
    truthy (PrimitiveFunction _ _) = True
interpret (Register r) = interpretRegAct pop push r
interpret (AST.Code c) = do
    closure <- close c
    push closure
interpret (LString s) = push (Str s)
interpret (LNumber n) = push (Number n)
interpret (NewFrame x) = localEnv newFrame $ interpret x
