{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Type(ASym(..), SSym(..), Arity(..), Action(..)) where

import Data.List

newtype ASym = ASymbol {asym :: String} deriving (Eq, Ord)
newtype SSym = SSymbol {ssym :: String} deriving (Eq, Ord)

instance Show ASym where
    show = asym

instance Show SSym where
    show = ssym

data Arity = Obj
    | Seq Action
    | ASym ASym
        deriving (Eq)

instance Show Arity where
    show Obj = "*"
    show (Seq s) = "<" ++ show s ++ ">"
    show (ASym a) = "?" ++ show a

data Action = SSym SSym
    | Appended [Action]
    | Push Arity
    | Pop Arity
        deriving (Eq)

instance Show Action where
    show (Push a) = "+" ++ show a
    show (Pop a) = "-" ++ show a
    show (SSym s) = "#" ++ show s
    show (Appended seqs) = "(" ++ intercalate "~" (fmap show seqs) ++ ")"
