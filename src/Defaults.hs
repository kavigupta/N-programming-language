module Defaults(indexBuiltinFunction) where

import Prelude hiding (lookup)
import Control.Monad.Except
import Data.Map hiding (foldr, map)
import Data.Char

import Environment

builtins :: Map String (InterpAct ())
builtins = fromList [
        ("+", numberOperator (+)),
        ("-", numberOperator (-)),
        ("*", numberOperator (*)),
        ("/", numberOperator div),
        ("?", ifthenelse),
        ("=", equality),
        ("l", list)
    ]

indexBuiltinFunction :: Bool -> String -> InterpAct Object
indexBuiltinFunction implicitLiteral name = case lookup name builtins of
    Nothing -> if implicitLiteral then return $ Str name else throwError $ UnboundVariable name
    Just x -> return . PrimitiveFunction name $ x

truthy :: Object -> Bool
truthy (Number x) = x /= 0
truthy (Str s) = s /= ""
truthy (Code c _) = c /= []
truthy Nil = False
truthy (Pair _ _) = True
truthy (PrimitiveFunction _ _) = True

ifthenelse :: InterpAct ()
ifthenelse = do
    condition <- pop
    consequent <- pop
    alternative <- pop
    push $ if truthy condition then consequent else alternative

equality :: InterpAct ()
equality = do
    lhs <- pop
    rhs <- pop
    areEq <- objEqual lhs rhs
    push . Number $ if areEq then 1 else 0

list :: InterpAct ()
list = do
        top <- pop
        case top of
            Number x
                | x < 0 -> err
                | otherwise -> do
                    items <- replicateM (fromInteger x) pop
                    push $ foldr Pair Nil items
            Nil -> push Nil
            (Pair a b) -> push $ dedotify a b
            Str s -> push $ foldr (Pair . cTn) Nil s
            _ -> err
    where
    err = throwError . BuiltinTypeError $ "#l requires an integer and then a sequence of elements or another container"
    cTn = Number . toInteger . ord

dedotify :: Object -> Object -> Object
dedotify car Nil = Pair car Nil
dedotify car (Pair cadr cddr) = Pair car $ dedotify cadr cddr
dedotify car cdr = Pair car (Pair cdr Nil)

numberOperator :: (Integer -> Integer -> Integer) -> InterpAct ()
numberOperator (#) = do
    x <- pop
    y <- pop
    case (x, y) of
        (Number x', Number y') -> push . Number $ x' # y'
        _ -> throwError . BuiltinTypeError $
                "Builtin requires two Integers but received " ++ show x ++ " and " ++ show y

