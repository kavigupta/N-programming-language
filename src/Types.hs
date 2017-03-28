{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Control.Monad.State
import Control.Monad.Except
import Control.Lens hiding (assignA)
import Data.Map(Map, insert, empty, findWithDefault)

newtype ASym = ASymbol {asym :: String} deriving (Eq, Ord, Show)
newtype SSym = SSymbol {ssym :: String} deriving (Eq, Ord, Show)

data Arity = Obj
    | Seq Sequence
    | ASym ASym
        deriving (Show, Eq)

data Action = Push Arity | Pop Arity
    deriving (Show, Eq)

data Sequence = Sequ [Action]
    | SSym SSym
    | Sequence :+: Sequence
        deriving (Show, Eq)

data Environment = Environment {_aMap :: Map ASym Arity, _sMap :: Map SSym Sequence} deriving Show

defaultEnvironment :: Environment
defaultEnvironment = Environment empty empty

makeLenses ''Environment

typeOfPlus :: Arity
typeOfPlus = Seq . Sequ $ [Pop Obj, Pop Obj, Push Obj]

sX :: Sequence
sX = SSym . SSymbol $ "x"

typeOfAppliedPlus :: Arity
typeOfAppliedPlus = Seq (Sequ [Push typeOfPlus, Pop (Seq sX)] :+: sX)

type Unifier = StateT Environment (Either UnificationError)

data UnificationError = NotImplemented | Impossible
    deriving Show

-- TODO circular dependencies?
assignA :: ASym -> Arity -> Unifier ()
assignA a v = aMap %= insert a v

assignS :: SSym -> Sequence -> Unifier ()
assignS s v = sMap %= insert s v

class Simplifiable u where
    simplify :: u -> Unifier u

class Unifiable u where
    unify :: u -> u -> Unifier ()

instance Unifiable Arity where
    unify (ASym a) v = assignA a v
    unify v (ASym a) = assignA a v
    unify Obj Obj = return ()
    unify (Seq a) (Seq b) = unify a b
    unify _ _ = throwError Impossible

instance Unifiable Sequence where
    unify (Sequ a) (Sequ b) = forM_ (zip a b) (uncurry unify)
    unify (SSym s) v = assignS s v
    unify v (SSym s) = assignS s v
    unify (_ :+: _) _ = error "Not Implemented +L"
    unify _ (_ :+: _) = error "Not Implemented +R"


instance Unifiable Action where
    unify (Push a) (Push b) = unify a b
    unify (Pop a) (Pop b) = unify a b
    unify _ _ = throwError Impossible

instance Simplifiable Arity where
    simplify Obj = return Obj
    simplify (Seq s) = Seq <$> simplify s
    simplify (ASym s) = do
        m <- use aMap
        return $ findWithDefault (ASym s) s m

simplifyCyclic :: (Eq a, Eq b, Simplifiable a, Simplifiable b) => (a, b) -> Unifier (a, b)
simplifyCyclic (a, b) = do
    a' <- simplify a
    b' <- simplify b
    if a' == a && b' == b then
        return (a', b')
    else
        simplifyCyclic (a', b')

instance Simplifiable Sequence where
    simplify (Sequ []) = return (Sequ [])
    simplify (Sequ (Push a:Pop a':rest)) = do
        unify a a'
        simplify (Sequ rest)
    simplify (Sequ (first:rest)) = do
        (first', rest') <- simplifyCyclic (first, Sequ rest)
        return $ case rest' of
            Sequ rest'' -> Sequ (first':rest'')
            other -> Sequ [first'] :+: other
    simplify (SSym s) = do
        m <- use sMap
        return $ findWithDefault (SSym s) s m
    simplify (a :+: b) = do
        (a', b') <- simplifyCyclic (a, b)
        return $ case (a', b') of
            (Sequ x, Sequ y) -> Sequ (x ++ y)
            (x, y) -> x :+: y

instance Simplifiable Action where
    simplify (Push x) = Push <$> simplify x
    simplify (Pop x) = Pop <$> simplify x
