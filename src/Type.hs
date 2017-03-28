{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Type(ASym(..), SSym(..), Arity(..), Action(..), Sequence(..)) where

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
