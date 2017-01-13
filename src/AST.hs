module AST(AST(..), printCode) where

import RegFile

data AST =
      Symbol Char
    | Definition
    | Index
    | Lookup
    | Duplicate
    | Execute
    | Self
    | Parents
    | Quine
    | Conditional
    | Register RegisterIndex
    | Code [AST]
    | LString String
    | LNumber Integer
    | NewFrame AST
        deriving (Show, Eq)

printCode :: AST -> String
printCode = show
