module AST(AST(..)) where

data AST =
      Symbol Char
    | Definition
    | Index
    | Lookup
    | Duplicate
    | Execute
    | Code AST
    | LString String
    | LNumber Integer
    | Sequence [AST]
    | NewFrame AST