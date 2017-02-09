module Main where

import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, digit, char)
-- import Text.Parsec.Char
import Text.Parsec.Combinator (many1, option)


int :: Parser Integer
int = do
    sign <- option '+' (char '-')
    n <- many1 digit
    return $ read n * (if sign == '+' then 1 else -1)

helpParse :: Parser a -> String -> Either ParseError a
helpParse p = parse p "Thom kan niet typen"

main :: IO ()
main = print "hello world"
