module Defaults(indexBuiltinFunction) where

import Control.Monad.Except

import Environment

indexBuiltinFunction :: String -> InterpAct Object
indexBuiltinFunction "+" = numberOperator "+" (+)
indexBuiltinFunction "-" = numberOperator "-" (-)
indexBuiltinFunction "*" = numberOperator "*" (*)
indexBuiltinFunction "/" = numberOperator "/" div
indexBuiltinFunction name = throwError $ UnboundVariable name

numberOperator :: String -> (Integer -> Integer -> Integer) -> InterpAct Object
numberOperator name (#) = return $ PrimitiveFunction name $ do
    x <- pop
    y <- pop
    case (x, y) of
        (Number x', Number y') -> push . Number $ x' # y'
        _ -> throwError . BuiltinTypeError $
                "Builtin " ++ show name ++ " requires two Integers but received " ++ show x ++ " and " ++ show y

