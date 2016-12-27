module Parser (parseN) where

import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token
import Text.Parsec
import Data.Functor.Identity

import AST

type Parser b = ParsecT String () Identity b

parseN :: String -> Either ParseError AST
parseN = parse nParser "N"

nParser :: Parser AST
nParser = do
    se <- many $ do
        spaces
        lString <|> lNumber <|> code <|> parenthesized <|> braced <|> atom
    return $ case se of
        [x] -> x
        xs -> Sequence xs

atom :: Parser AST
atom = do
    c <- noneOf "(){}`'"
    case c of
        '@' -> return Definition
        '&' -> return Index
        '~' -> return Lookup
        ';' -> return Duplicate
        '$' -> return Execute
        x   -> return $ Symbol x

lString, code, parenthesized, braced, lNumber :: Parser AST
lString = LString <$> stringLiteral (makeTokenParser haskellDef)
code = surrounded '`' '\'' Code
parenthesized = surrounded '(' ')' id
braced = surrounded '{' '}' NewFrame
lNumber = LNumber . read <$> (return <$> digit <|> multiDigit)
    where
    multiDigit = do
        char '#'
        opt <- option "" $ do
            char '-'
            return "-"
        digits <- many digit
        return $ opt ++ digits

surrounded :: Char -> Char -> (AST -> x) -> Parser x
surrounded start end f = do
    char start
    contents <- nParser
    char end
    return . f $ contents
