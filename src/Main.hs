module Main where

import Text.Parsec (parse, ParseError, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, digit, char, letter, alphaNum)
-- import Text.Parsec.Char
import Text.Parsec.Combinator (many1, option, manyTill, notFollowedBy)


int :: Parser Integer
int = do
    sign <- option '+' (char '-')
    n <- many1 digit
    return $ read n * (if sign == '+' then 1 else -1)

identifier :: Parser String
identifier = do
    firstchar <- letter
    rest <- manyTill restChars (notFollowedBy restChars)
    return $ firstchar : rest
    where
        restChars = char '_' <|> alphaNum

helpParse :: Parser a -> String -> Either ParseError a
helpParse p = parse p "Thom kan niet typen"

main :: IO ()
main = print "hello world"
