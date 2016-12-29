module AST(AST(..), printCode) where

data AST =
      Symbol Char
    | Definition
    | Index
    | Lookup
    | Duplicate
    | Execute
    | Self
    | Quine
    | Code [AST]
    | LString String
    | LNumber Integer
    | NewFrame AST
        deriving (Show, Eq)

printCode :: AST -> String
printCode = show