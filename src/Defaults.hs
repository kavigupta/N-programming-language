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
        ("=", equality <|> error "Unreachable (=)"),
        ("[", indexL <|> indexS <|> throwError (BuiltinTypeError "Invalid Attempt to index")),
        ("l", list),
        ("r", range <|> throwError (BuiltinTypeError "#r requires two numbers to produce a range")),
        ("c", map chr <|> (pop >>= \e -> throwError . BuiltinTypeError $ "Non-codepoint in toString " ++ show e)),
        ("·", return ()),
        ("p", typedPrint <|> error "Unreachable (p)"),
        ("i", input),
        (",", cons <|> error "Unreachable (,)"),
        (".", deCons <|> (pop >>= \o -> throwError . BuiltinTypeError $ "Tried to unpack " ++ show o))
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

equality :: TwoStack Object Object -> Integer
equality (TwoStack lhs rhs)
    | objEqual lhs rhs = 1
equality _ = 0

list :: InterpAct ()
list = (id :: [Object] -> [Object]) <|> nList
    where
    nList = do
        top <- pop
        case top of
            Number x
                | x < 0 -> throwError . BuiltinTypeError $ "#l cannot be called on a negative"
                | otherwise -> do
                    items <- replicateM (fromInteger x) pop
                    toStack items
            x -> throwError . BuiltinTypeError $ "Unable to listify " ++ show x

range :: TwoStack Integer Integer -> [Integer]
range (TwoStack l h) = [l..h]

sub :: InterpAct ()
sub = stackCurry ((-) :: Integer -> Integer -> Integer) <|> diff <|> err
    where
    err = pop >>= \x -> throwError . BuiltinTypeError $ "Unable to listify " ++ show x

diff :: TwoStack [Object] [Object] -> [Object]
diff (TwoStack x y) = filter (`notIn` y) x
    where
    notIn :: Object -> [Object] -> Bool
    notIn = (not .) . any . objEqual

indexS :: TwoStack Int String -> String
indexS = (return .) . stackCurry . flip $ (!!)

indexL :: TwoStack Int [Object] -> Object
indexL = stackCurry . flip $ (!!)

typedPrint :: Object -> IO NoStack
typedPrint (Str s) = putStrLn s >> return NoStack
typedPrint u = print u >> return NoStack

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

deCons :: (Object, Object) -> TwoStack Object Object
deCons (x, y) = TwoStack x y

dedotify :: Object -> Object -> [Object]
dedotify car Nil = [car]
dedotify car (Pair cadr cddr) = car : dedotify cadr cddr
dedotify car cdr = [car, cdr]

stackCurry :: (a -> b -> c) -> TwoStack a b -> c
stackCurry f (TwoStack x y) = f x y

numberOperator :: (Integer -> Integer -> Integer) -> InterpAct ()
numberOperator (#) = stackCurry (#) <|> err
    where err = pop >>= \x -> pop >>= \y -> throwError . BuiltinTypeError $
                "Builtin requires two Integers but received " ++ show x ++ " and " ++ show y

class ToObject a where
    toObject :: a -> Object

instance (ToObject a) => ToStack a where
    toStack = push . toObject

class ToStack a where
    toStack :: a -> InterpAct ()

data TwoStack a b = TwoStack a b

data NoStack = NoStack

instance (ToStack a, ToStack b) => ToStack (TwoStack a b) where
    toStack (TwoStack x y) = toStack x >> toStack y

instance ToStack NoStack where
    toStack NoStack = return ()

instance (ToStack a) => ToStack (IO a) where
    toStack v = liftIO v >>= toStack

instance ToObject Integer where
    toObject = Number

instance ToObject Object where
    toObject = id

instance ToObject String where
    toObject = Str

instance (ToObject a, ToObject b) => ToObject (a, b) where
    toObject (a, b) = Pair (toObject a) (toObject b)

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

instance (FromObject Int) where
    fromObject = fmap fromInteger . fromObject

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
        return $ liftM2 TwoStack x y

instance (FromObject a, FromObject b) => (FromObject (a, b)) where
    fromObject (Pair b a) = liftM2 (,) (fromObject a) (fromObject b)
    fromObject _ = Nothing

infixr 9 <|>
(<|>) :: (FromStack a, ToStack b) => (a -> b) -> InterpAct () -> InterpAct ()
f <|> other = do
    first <- fromStack
    case first of
        Just v -> toStack $ f v
        Nothing -> other
