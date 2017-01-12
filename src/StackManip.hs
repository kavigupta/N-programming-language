{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, IncoherentInstances #-}
module StackManip(ToStack(..), TwoStack(..), (<|>)) where

import Control.Monad.Except
import Data.Char

import Environment

data TwoStack a b = TwoStack a b

class ToObject a where
    toObject :: a -> Object

class ToStack a where
    toStack :: a -> InterpAct ()

class FromObject a where
    fromObject :: Object -> Maybe a

class FromStack a where
    fromStack :: InterpAct (Maybe a)

infixr 9 <|>
(<|>) :: (FromStack a, ToStack b) => (a -> b) -> InterpAct () -> InterpAct ()
f <|> other = do
    first <- fromStack
    case first of
        Just v -> toStack $ f v
        Nothing -> other

instance (ToObject a) => ToStack a where
    toStack = push . toObject

instance (ToStack a, ToStack b) => ToStack (TwoStack a b) where
    toStack (TwoStack x y) = toStack x >> toStack y

instance ToStack () where
    toStack = return

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

dedotify :: Object -> Object -> [Object]
dedotify car Nil = [car]
dedotify car (Pair cadr cddr) = car : dedotify cadr cddr
dedotify car cdr = [car, cdr]
