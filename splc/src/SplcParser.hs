module SplcParser where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

-- eats spaces and comments
sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment
    where
        lineComment = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

-- parser wrapper so it will ignore spaces
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parses individual characters
-- but meanwhile ignores whitespaces that follow it
symbol :: String -> Parser String
symbol = L.symbol sc

-- Parse an integer
int :: Parser Integer
int = L.signed (void $ string "") (lexeme L.integer)

-- Reserved words
reserved :: [String]
reserved = ["if", "else", "while", "return", "True", "False"]

-- Parse an identifier
identifier :: Parser String
identifier = (lexeme . try) (parser >>= check)
    where
        -- (:) adds the parsed first letter to the list parsed by the many
        parser = (:) <$> letterChar <*> many (char '_' <|> alphaNumChar)
        check :: String -> Parser String
        check x = if x `elem` reserved
                    then fail $ show x ++ " is a reserved keyword"
                    else return x
