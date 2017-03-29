{-# LANGUAGE TemplateHaskell#-}
module TypeManipulation where

import Type

import Control.Monad.State
import Control.Monad.Except
import Control.Lens hiding (assignA)
import Data.Map(Map, insert, empty, findWithDefault, toList)

data Environment = Environment {_aMap :: Map ASym Arity, _sMap :: Map SSym Action}

instance Show Environment where
    show (Environment a s) = "{" ++ unwords (map show (toList a) ++ map show (toList s)) ++ "}"

defaultEnvironment :: Environment
defaultEnvironment = Environment empty empty

makeLenses ''Environment

type Unifier = StateT Environment (Either UnificationError)

runUnifierT :: Unifier x -> Either UnificationError (x, Environment)
runUnifierT = flip runStateT defaultEnvironment

data UnificationError = NotImplemented | Impossible | NoTypeApplicable
    deriving Show

-- TODO circular dependencies?
assignA :: ASym -> Arity -> Unifier ()
assignA a v = aMap %= insert a v

assignS :: SSym -> Action -> Unifier ()
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

instance Unifiable Action where
    unify (Appended a) (Appended b) = forM_ (zip a b) (uncurry unify)
    unify (SSym s) v = assignS s v
    unify v (SSym s) = assignS s v
    unify (Push a) (Push b) = unify a b
    unify (Pop a) (Pop b) = unify a b
    unify _ _ = throwError Impossible

instance Simplifiable Arity where
    simplify Obj = return Obj
    simplify (Seq s) = Seq <$> simplify s
    simplify (ASym s) = do
        m <- use aMap
        return $ findWithDefault (ASym s) s m

instance (Eq a, Eq b, Simplifiable a, Simplifiable b) => Simplifiable (a, b) where
    simplify (a, b) = do
        a' <- simplify a
        b' <- simplify b
        if a' == a && b' == b then
            return (a', b')
        else
            simplify (a', b')

instance (Eq a, Simplifiable a) => Simplifiable [a] where
    simplify xs = do
        xs' <- mapM simplify xs
        if xs == xs' then
            return xs'
        else
            simplify xs'

instance Simplifiable Action where
    simplify (SSym s) = do
        m <- use sMap
        return $ findWithDefault (SSym s) s m
    simplify (Appended (Push a:Pop b:xs)) = do
        unify a b
        simplify $ Appended xs
    simplify (Appended items) = do
        items' <- simplify items
        let result = appendSeqs items'
        result' <- case result of
            Appended (x:xs) -> do
                r <- simplify $ Appended xs
                return $ appendSeqs [x, r]
            u -> return u
        (if result' == Appended items then return else simplify) result'
    simplify (Push x) = Push <$> simplify x
    simplify (Pop x) = Pop <$> simplify x

appendSeqs :: [Action] -> Action
appendSeqs [] = Appended []
appendSeqs [x] = x
appendSeqs (Appended a:Appended b:xs) = appendSeqs (Appended (a ++ b) : xs)
appendSeqs (Appended a:b:xs) = case appendSeqs (b:xs) of
    (Appended (Appended u:v)) -> appendSeqs (Appended (a ++ u) : v)
    y -> y
appendSeqs (a:b:xs) = case appendSeqs (b:xs) of
    Appended res -> Appended (a : res)
    y -> Appended [a, y]
