module Defaults(indexBuiltinFunction) where

import Prelude hiding (lookup)
import Control.Monad.Except
import Data.Map hiding (foldr, map, (\\), filter)
import Data.Char

import Environment
import Parser

builtins :: Map String (InterpAct ())
builtins = fromList [
        ("`", void pop),
        ("+", numberOperator (+)),
        ("-", sub),
        ("*", numberOperator (*)),
        ("/", numberOperator div),
        ("=", equality),
        ("l", list),
        ("r", range),
        ("c", string),
        ("·", return ()),
        ("p", typedPrint),
        ("i", input),
        (",", cons),
        (".", deCons),
        ("n", nullN)
    ]
library :: Map String String
library = fromList [
        ("!", "{N@(N1N-$1¦*$)(1)0N=$?$}$"),
        ("e", "0r$"),
        ("∑", "0{N@(.$N+$1¦)(`N)2&n$?$}$")
    ]

indexBuiltinFunction :: Bool -> String -> InterpAct Object
indexBuiltinFunction implicitLiteral name = case lookup name builtins of
    Just x -> return . PrimitiveFunction name $ x
    Nothing -> case lookup name library of
        Just x -> case parseN x of
            Left err -> throwError $ LibraryError err
            Right ast -> do
                e <- getEnv
                return $ Code ast e
        Nothing -> if implicitLiteral then return $ Str name else throwError $ UnboundVariable name

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
                    push $ toObject items
            x -> toObject <$> listify x >>= push
    where
    err = throwError . BuiltinTypeError $ "#l cannot be called on a negative"

range :: InterpAct ()
range = do
    lo <- pop
    hi <- pop
    case (lo, hi) of
        (Number l, Number h) -> push $ toObject [l..h]
        _ -> throwError . BuiltinTypeError $ "#r requires two numbers to produce a range"

sub :: InterpAct ()
sub = do
    x' <- pop
    y' <- pop
    case (x', y') of
        (Number x, Number y) -> push . Number $ x - y
        _ -> do
            x <- listify x'
            y <- listify y'
            filterd <- filterM (notIn y) x
            push . toObject $ filterd
    where
    notIn :: [Object] -> Object -> InterpAct Bool
    notIn [] _ = return True
    notIn (x:xs) y = do
        v <- objEqual x y
        if v then return False else notIn xs y

listify :: Object -> InterpAct [Object]
listify Nil = return []
listify (Pair a b) = return $ dedotify a b
listify (Str s) = return $ map cTn s
    where
    cTn = Number . toInteger . ord
listify x = throwError . BuiltinTypeError $ "Unable to listify " ++ show x

string :: InterpAct ()
string = do
        x <- pop
        lst <- listify x
        str <- mapM toCharacter lst
        push . Str $ str
    where
    toCharacter :: Object -> InterpAct Char
    toCharacter (Number e') = return $ chr . fromInteger $ e'
    toCharacter e = throwError . BuiltinTypeError $ "Non-codepoint in toString " ++ show e

typedPrint :: InterpAct ()
typedPrint = do
    x <- pop
    case x of
        Str s -> liftIO $ putStrLn s
        u -> liftIO $ print u

input :: InterpAct ()
input = do
    mode <- pop
    line <- liftIO getLine
    case mode of
        Number 0 -> push . Str $ line
        Number 1 -> push . Number . read $ line
        _ -> throwError . BuiltinTypeError $ "Invalid read mode " ++ show mode

cons :: InterpAct ()
cons = do
    car <- pop
    cdr <- pop
    push $ Pair car cdr

deCons :: InterpAct ()
deCons = do
    lis <- pop
    case lis of
        Pair car cdr -> push cdr >> push car
        o -> throwError . BuiltinTypeError $ "Tried to unpack " ++ show o

nullN :: InterpAct ()
nullN = do
    lis <- pop
    push . Number $ case lis of
        Nil -> 1
        _ -> 0

dedotify :: Object -> Object -> [Object]
dedotify car Nil = [car]
dedotify car (Pair cadr cddr) = car : dedotify cadr cddr
dedotify car cdr = [car, cdr]

numberOperator :: (Integer -> Integer -> Integer) -> InterpAct ()
numberOperator (#) = do
    x <- pop
    y <- pop
    case (x, y) of
        (Number x', Number y') -> push . Number $ x' # y'
        _ -> throwError . BuiltinTypeError $
                "Builtin requires two Integers but received " ++ show x ++ " and " ++ show y

instance Objectifiable Integer where
    toObject = Number

class Objectifiable a where
    toObject :: a -> Object

instance Objectifiable Object where
    toObject = id

instance (Objectifiable a) => Objectifiable [a] where
    toObject = foldr (Pair . toObject) Nil
