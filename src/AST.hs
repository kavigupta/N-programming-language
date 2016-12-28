module AST(AST(..), printCode) where

data AST =
      Symbol Char
    | Definition
    | Index
    | Lookup
    | Duplicate
    | Execute
    | Self
    | Code [AST]
    | LString String
    | LNumber Integer
    | NewFrame AST
        deriving (Show)

printCode :: AST -> String
printCode = show