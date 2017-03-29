{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module TypeCheck where

import Type
import TypeManipulation
import AST

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

data UnTypeable = UnTypeable

type GeneratorT u m = StateT Integer m

class Indexable u where
    fromIndex :: Integer -> u

generate :: (Indexable u, Monad m) => GeneratorT u m u
generate = do
    n <- get
    put $ n + 1
    return $ fromIndex n

generateS :: (Monad m) => GeneratorT String m Sequence
generateS = SSym . SSymbol <$> generate

generateA :: (Monad m) => GeneratorT String m Arity
generateA = ASym . ASymbol <$> generate

type Typer = GeneratorT String (ExceptT UnTypeable (ReaderT (Maybe AST) Unifier))

simplifiedType :: AST -> Either UnificationError (Arity, Environment)
simplifiedType ast = runUnifierT $ do
    ari <- arityType ast
    case ari of
        Left UnTypeable -> throwError NoTypeApplicable
        Right arity -> simplify arity

arityType :: AST -> Unifier (Either UnTypeable Arity)
arityType = (fmap . fmap) Seq . flip runReaderT Nothing . runExceptT . flip evalStateT 0 . sequenceType

sequenceType :: AST -> Typer Sequence
sequenceType (Symbol _) = throwError UnTypeable
sequenceType Definition = throwError UnTypeable
sequenceType Index = do
    previous <- ask
    case previous of
        Just (LNumber n) | n >= 0 -> Sequ . ([Pop Obj] ++) <$> indexType (fromInteger n)
        _ -> throwError UnTypeable
sequenceType Lookup = throwError UnTypeable
sequenceType Duplicate = Sequ <$> indexType 1
sequenceType Execute = do
    var <- generateS
    return $ Sequ [Pop (Seq var)] :+: var
sequenceType Self = throwError UnTypeable
sequenceType Parents = throwError UnTypeable
sequenceType Quine = return . Sequ $ [Push Obj]
sequenceType Conditional = do
    var <- generateS
    return $ Sequ [Pop (Seq var), Pop (Seq var), Pop Obj] :+: var
sequenceType Swap = do
    a <- generateS
    b <- generateS
    return . Sequ $ [Pop (Seq a), Pop (Seq b), Push (Seq a), Push (Seq b)]
sequenceType (Register _) = throwError UnTypeable
sequenceType (Code ast) = do
    types <- forM (zip ast (Nothing : map Just ast)) $ \(value, previous) -> local (const previous) $ sequenceType value
    return $ foldr (:+:) (Sequ []) types
sequenceType (LString _) = return $ Sequ [Push Obj]
sequenceType (LNumber _) = return $ Sequ [Push Obj]
sequenceType (NewFrame x) = sequenceType x -- TODO update if/when implementing variables

instance Indexable String where
    fromIndex = show

indexType :: Int -> Typer [Action]
indexType 0 = return []
indexType n = do
    vars <- replicateM n generateA
    let popElems = Push <$> vars
    let pushElems = Pop <$> reverse vars
    return $ popElems ++ pushElems ++ [head pushElems]
    -- n = 3 --> -*, -(x), -(y), -(z), +(z), +(y), +(x), +(z)
