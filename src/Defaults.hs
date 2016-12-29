module Defaults(indexBuiltinFunction) where

import Control.Monad.Except

import Environment

indexBuiltinFunction :: Bool -> String -> InterpAct Object
indexBuiltinFunction _ "+" = numberOperator "+" (+)
indexBuiltinFunction _ "-" = numberOperator "-" (-)
indexBuiltinFunction _ "*" = numberOperator "*" (*)
indexBuiltinFunction _ "/" = numberOperator "/" div
indexBuiltinFunction _ "?" = return . PrimitiveFunction "?" $ do
    condition <- pop
    consequent <- pop
    alternative <- pop
    push $ if truthy condition then consequent else alternative
indexBuiltinFunction _ "=" = return . PrimitiveFunction "=" $ do
    lhs <- pop
    rhs <- pop
    equality <- objEqual lhs rhs
    push . Number $ if equality then 1 else 0
indexBuiltinFunction True name = return $ Str name
indexBuiltinFunction False name = throwError $ UnboundVariable name

truthy :: Object -> Bool
truthy (Number x) = x /= 0
truthy (Str s) = s /= ""
truthy (Code c _) = c /= []
truthy Nil = False
truthy (Pair _ _) = True
truthy (PrimitiveFunction _ _) = True

numberOperator :: String -> (Integer -> Integer -> Integer) -> InterpAct Object
numberOperator name (#) = return . PrimitiveFunction name $ do
    x <- pop
    y <- pop
    case (x, y) of
        (Number x', Number y') -> push . Number $ x' # y'
        _ -> throwError . BuiltinTypeError $
                "Builtin " ++ show name ++ " requires two Integers but received " ++ show x ++ " and " ++ show y

