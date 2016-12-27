module AST(ParsedAST(..)) where

data ParsedAST =
      ParsedAtom Char
    | ParsedLString String
    | ParsedCode ParsedAST
    | Parenthesized ParsedAST
    | Braced ParsedAST
    | ParsedSequence [ParsedAST]
        deriving Show
