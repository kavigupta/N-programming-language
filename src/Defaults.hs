module Defaults(indexBuiltinFunction) where

import Prelude hiding (lookup)
import Control.Monad.Except
import Data.Map hiding (foldr, map, (\\), filter)
import Data.Char

import Environment
import Parser
import StackManip
import Object
import Error

builtins :: Map String (InterpAct ())
builtins = fromList [
        ("`", void pop),
        ("∥", void (pop >> pop)),
        ("+", numberOperator (+)),
        ("-", sub),
        ("d", subtract (1 :: Integer) <|> builtInError "Attempted to Decrement non number"),
        ("*", numberOperator (*)),
        ("/", numberOperator div),
        ("%", numberOperator mod),
        ("<", numberOperator (\x y -> if x < y then 1 else 0)),
        (">", numberOperator (\x y -> if x > y then 1 else 0)),
        ("=", equality <|> error "Unreachable (=)"),
        ("[", indexL <|> indexS <|> builtInError "Invalid Attempt to index"),
        ("l", list),
        ("r", range <|> builtInError "#r requires two numbers to produce a range"),
        ("c", map chr <|> (pop >>= \e -> builtInError $ "Non-codepoint in toString " ++ show e)),
        ("·", return ()),
        ("p", typedPrint <|> error "Unreachable (p)"),
        ("i", input),
        (",", cons <|> error "Unreachable (,)"),
        (".", deCons <|> (pop >>= \o -> builtInError $ "Tried to unpack " ++ show o))
    ]

library :: Map String String
library = fromList [
        ("!", "{N@0N=$(1)(N1N-$1¦*$)?$}$"),
        ("e", "0r$"),
        ("w", "1&(β$β1¦)∥?$"),
        ("∑", "0{N@2&n$(`N)(.$N+$1¦)?$}$")
    ]

indexBuiltinFunction :: Bool -> String -> InterpAct FObject
indexBuiltinFunction _ "n" = return Nil
indexBuiltinFunction implicitLiteral name = case lookup name builtins of
    Just x -> return . PrimitiveFunction name $ x
    Nothing -> case lookup name library of
        Just x -> case parseN x of
            Left err -> libraryError err
            Right ast -> close ast
        Nothing -> if implicitLiteral then return $ Str name else unboundVariable name

equality :: TwoStack FObject FObject -> Integer
equality (TwoStack lhs rhs)
    | objEqual lhs rhs = 1
equality _ = 0

list :: InterpAct ()
list = (id :: [FObject] -> [FObject]) <|> nList
    where
    nList = do
        top <- pop
        case top of
            Number x
                | x < 0 -> builtInError "#l cannot be called on a negative"
                | otherwise -> do
                    items <- replicateM (fromInteger x) pop
                    toStack items
            x -> builtInError $ "Unable to listify " ++ show x

range :: TwoStack Integer Integer -> [Integer]
range (TwoStack l h) = [l..h]

sub :: InterpAct ()
sub = stackCurry ((-) :: Integer -> Integer -> Integer) <|> diff <|> err
    where
    err = pop >>= \x -> builtInError $ "Unable to listify " ++ show x

diff :: TwoStack [FObject] [FObject] -> [FObject]
diff (TwoStack x y) = filter (`notIn` y) x
    where
    notIn :: FObject -> [FObject] -> Bool
    notIn = (not .) . any . objEqual

indexS :: TwoStack Int String -> String
indexS = (return .) . stackCurry . flip $ (!!)

indexL :: TwoStack Int [FObject] -> FObject
indexL = stackCurry . flip $ (!!)

typedPrint :: FObject -> IO ()
typedPrint (Str s) = putStrLn s
typedPrint u = print u

input :: InterpAct ()
input = do
    mode <- pop
    line <- liftIO getLine
    case mode of
        Number 0 -> push . Str $ line
        Number 1 -> push . Number . read $ line
        _ -> builtInError $ "Invalid read mode " ++ show mode

cons :: TwoStack FObject FObject -> (FObject, FObject)
cons (TwoStack x y) = (x, y)

deCons :: (FObject, FObject) -> TwoStack FObject FObject
deCons (x, y) = TwoStack x y

stackCurry :: (a -> b -> c) -> TwoStack a b -> c
stackCurry f (TwoStack x y) = f x y

numberOperator :: (Integer -> Integer -> Integer) -> InterpAct ()
numberOperator (#) = stackCurry (#) <|> err
    where err = pop >>= \x -> pop >>= \y -> builtInError $
                "Builtin requires two Integers but received " ++ show x ++ " and " ++ show y
