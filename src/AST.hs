module AST(ParsedAST(..)) where

data ParsedAST =
      ParsedAtom Char
    | ParsedLString String
    | ParsedLNumber Integer
    | ParsedCode ParsedAST
    | Parenthesized ParsedAST
    | Braced ParsedAST
    | ParsedSequence [ParsedAST]
        deriving Show
