{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, IncoherentInstances #-}
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
        ("%", numberOperator mod),
        ("<", numberOperator (\x y -> if x < y then 1 else 0)),
        (">", numberOperator (\x y -> if x > y then 1 else 0)),
        ("=", equality),
        -- ("[", index),
        ("l", list),
        ("r", range),
        ("c", string),
        ("·", return ()),
        ("p", typedPrint),
        ("i", input),
        (",", cons <|> error "Unreachable"),
        (".", deCons)
    ]
library :: Map String String
library = fromList [
        ("!", "{N@0N=$(1)(N1N-$1¦*$)?$}$"),
        ("e", "0r$"),
        ("∑", "0{N@2&n$(`N)(.$N+$1¦)?$}$")
    ]

indexBuiltinFunction :: Bool -> String -> InterpAct Object
indexBuiltinFunction _ "n" = return Nil
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
                    toStack items
            x -> case fromObject x :: Maybe [Object] of
                Just x' -> toStack x'
                Nothing -> throwError . BuiltinTypeError $ "Unable to listify " ++ show x
    where
    err = throwError . BuiltinTypeError $ "#l cannot be called on a negative"

range :: InterpAct ()
range = do
    lo <- pop
    hi <- pop
    case (lo, hi) of
        (Number l, Number h) -> toStack [l..h]
        _ -> throwError . BuiltinTypeError $ "#r requires two numbers to produce a range"

sub :: InterpAct ()
sub = do
    x' <- pop
    y' <- pop
    case (x', y') of
        (Number x, Number y) -> push . Number $ x - y
        _ -> case differ x' y' of
                Just (x, y) -> filterM (notIn y) x >>= toStack
                Nothing -> pop >>= \x -> throwError . BuiltinTypeError $ "Unable to listify " ++ show x
    where
    differ x' y' = do
        x <- fromObject x'
        y <- fromObject y'
        return (x, y)
    notIn :: [Object] -> Object -> InterpAct Bool
    notIn [] _ = return True
    notIn (x:xs) y = do
        v <- objEqual x y
        if v then return False else notIn xs y


string :: InterpAct ()
string = do
        x <- pop
        case fromObject x of
            Just lst -> do
                str <- mapM toCharacter lst
                push . Str $ str
            Nothing -> error "Unreachable"
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

cons :: TwoStack Object Object -> (Object, Object)
cons (TwoStack x y) = (x, y)

deCons :: InterpAct ()
deCons = deConsF <|> err
    where
    deConsF :: (Object, Object) -> TwoStack Object Object
    deConsF (x, y) = TwoStack x y
    err = pop >>= \o -> throwError . BuiltinTypeError $ "Tried to unpack " ++ show o

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

class ToObject a where
    toObject :: a -> Object

instance (ToObject a) => ToStack a where
    toStack = push . toObject

class ToStack a where
    toStack :: a -> InterpAct ()

data TwoStack a b = TwoStack a b

instance (ToStack a, ToStack b) => ToStack (TwoStack a b) where
    toStack (TwoStack x y) = toStack x >> toStack y

instance ToObject Integer where
    toObject = Number

instance ToObject Object where
    toObject = id

instance ToObject String where
    toObject = Str

instance (ToObject a, ToObject b) => ToObject (a, b) where
    toObject (b, a) = Pair (toObject a) (toObject b)

instance (ToObject a) => ToObject [a] where
    toObject = foldr (Pair . toObject) Nil

class FromObject a where
    fromObject :: Object -> Maybe a

class FromStack a where
    fromStack :: InterpAct (Maybe a)

instance {-# OVERLAPPABLE #-} (FromObject a) => FromStack a where
    fromStack = do
        x <- pop
        case fromObject x of
            Just y -> return (Just y)
            Nothing -> push x >> return Nothing

instance (FromObject Object) where
    fromObject = Just

instance (FromObject Integer) where
    fromObject (Number x') = Just x'
    fromObject _ = Nothing

instance (FromObject String) where
    fromObject (Str x') = Just x'
    fromObject _ = Nothing

instance (FromObject a) => FromObject [a] where
    fromObject Nil = return []
    fromObject (Pair a b) = mapM fromObject $ dedotify a b
    fromObject (Str s) = mapM (fromObject . cTn) s
        where
        cTn = Number . toInteger . ord
    fromObject _ = Nothing

instance (FromStack a, FromStack b) => (FromStack (TwoStack a b)) where
    fromStack = do
        x <- fromStack
        y <- fromStack
        return $ liftM2 TwoStack y x

instance (FromObject a, FromObject b) => (FromObject (a, b)) where
    fromObject (Pair b a) = liftM2 (,) (fromObject a) (fromObject b)
    fromObject _ = Nothing

(<|>) :: (FromStack a, ToStack b) => (a -> b) -> InterpAct () -> InterpAct ()
f <|> other = do
    first <- fromStack
    case first of
        Just v -> toStack $ f v
        Nothing -> other
