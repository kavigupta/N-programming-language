module Parser (parseN) where

import Text.Parsec.Language (haskellDef)
import Text.Parsec.Token
import Text.Parsec
import Data.Functor.Identity

import AST
import RegFile

type Parser b = ParsecT String () Identity b

parseN :: String -> Either ParseError [AST]
parseN = parse nParser "N"

nParser :: Parser [AST]
nParser = many $ do
        spaces
        lString <|> lNumber <|> code <|> braced <|> atom
atom :: Parser AST
atom = do
    c <- noneOf "(){}#"
    case c of
        '@' -> return Definition
        '&' -> return Index
        'ω' -> return Swap
        '~' -> return Lookup
        ';' -> return Duplicate
        '|' -> return Self
        '¦' -> return Parents
        '$' -> return Execute
        'q' -> return Quine
        '?' -> return Conditional
        'α' -> return $ Register Alpha
        'β' -> return $ Register Beta
        'γ' -> return $ Register Gamma
        'δ' -> return $ Register Delta
        x   -> return $ Symbol x

lString, code, braced, lNumber :: Parser AST
lString = LString <$> stringLiteral (makeTokenParser haskellDef)
code = surrounded '(' ')' Code
braced = surrounded '{' '}' (NewFrame . Code)
lNumber = LNumber . read <$> (return <$> digit <|> multiDigit)
    where
    multiDigit = do
        char '#'
        opt <- option "" $ do
            char '-'
            return "-"
        digits <- many digit
        return $ opt ++ digits

surrounded :: Char -> Char -> ([AST] -> x) -> Parser x
surrounded start end f = do
    char start
    contents <- nParser
    char end
    return . f $ contents
