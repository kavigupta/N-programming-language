module Defaults(indexBuiltinFunction) where

import Prelude hiding (lookup)
import Control.Monad.Except
import Data.Map

import Environment

builtins :: Map String (InterpAct ())
builtins = fromList [
        ("+", numberOperator (+)),
        ("-", numberOperator (-)),
        ("*", numberOperator (*)),
        ("/", numberOperator div),
        ("?", ifthenelse),
        ("=", equality)
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

numberOperator :: (Integer -> Integer -> Integer) -> InterpAct ()
numberOperator (#) = do
    x <- pop
    y <- pop
    case (x, y) of
        (Number x', Number y') -> push . Number $ x' # y'
        _ -> throwError . BuiltinTypeError $
                "Builtin requires two Integers but received " ++ show x ++ " and " ++ show y

