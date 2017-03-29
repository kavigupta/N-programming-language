{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Type(ASym(..), SSym(..), Arity(..), Action(..), Sequence(..)) where

newtype ASym = ASymbol {asym :: String} deriving (Eq, Ord)
newtype SSym = SSymbol {ssym :: String} deriving (Eq, Ord)

instance Show ASym where
    show = asym

instance Show SSym where
    show = ssym

data Arity = Obj
    | Seq Sequence
    | ASym ASym
        deriving (Eq)

instance Show Arity where
    show Obj = "*"
    show (Seq s) = "<" ++ show s ++ ">"
    show (ASym a) = "?" ++ show a

data Action = Push Arity | Pop Arity
    deriving (Eq)

instance Show Action where
    show (Push a) = "+" ++ show a
    show (Pop a) = "-" ++ show a

data Sequence = Sequ [Action]
    | SSym SSym
    | Sequence :+: Sequence
        deriving (Eq)

instance Show Sequence where
    show (Sequ acts) = "(" ++ unwords (map show acts) ++ ")"
    show (SSym s) = "#" ++ show s
    show (l :+: r) = show l ++ "~" ++ show r
