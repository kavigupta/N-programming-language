module Parser (parseN) where

import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token
import Text.Parsec
import Data.Functor.Identity

import AST

type Parser b = ParsecT String () Identity b

parseN :: Parser ParsedAST
parseN = do
    se <- many $ do
        spaces
        lString <|> lNumber <|> code <|> parenthesized <|> braced <|> atom
    return $ case se of
        [x] -> x
        xs -> ParsedSequence xs
    
atom :: Parser ParsedAST
atom = ParsedAtom <$> noneOf "(){}`'"

lString, code, parenthesized, braced, lNumber :: Parser ParsedAST
lString = ParsedLString <$> stringLiteral (makeTokenParser haskellDef)
code = surrounded '`' '\'' ParsedCode
parenthesized = surrounded '(' ')' Parenthesized
braced = surrounded '{' '}' Braced
lNumber = ParsedLNumber . read <$> (return <$> digit <|> multiDigit)
    where
    multiDigit = do
        char '#'
        opt <- option "" $ do
            char '-'
            return "-"
        digits <- many digit
        return $ opt ++ digits

surrounded :: Char -> Char -> (ParsedAST -> x) -> Parser x
surrounded start end f = do
    char start
    contents <- parseN
    char end
    return . f $ contents
