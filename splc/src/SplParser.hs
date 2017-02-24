module SplParser where

import Control.Monad (void, when)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import SplAST


spl :: Parser Spl
spl = Spl <$> parseDecls <* eof

parseDecls :: Parser [SplDecl]
parseDecls = some $
        (try ((SplDeclVar <$> varDecl) <?> "Variable definition"))
    <|> (try (funDecl <?> "Function definition"))

-- variable declarations
varDecl :: Parser SplVarDecl
varDecl = do
    t <- ((readWord "var" *> return SplTypeUnknown) <|> parseType) <?> "var or a type specification"
    i <- identifier
    _ <- symbol "="
    e <- expr
    _ <- symbol ";"
    return $ SplVarDecl t i e

-- function declarations
funDecl :: Parser SplDecl
funDecl = do
    name <- identifier
    args <- between (symbol "(") (symbol ")") $ identifier `sepBy` symbol ","
    (argTypes, retType) <- (
        option ([], SplRetType SplTypeUnknown)
            $ string "::" *> sc *> parseFunTypes)

    -- check if num args matches num types if given
    when (not (null argTypes) && length argTypes /= length args) $
        fail "Number of arguments and types does not match"
    _ <- symbol "{"
    varDecls <- many varDecl -- VarDecl*
    stmts <- some stmt  -- Stmt+
    _ <- symbol "}"
    return $ SplDeclFun name args argTypes retType varDecls stmts
    where
        parseFunTypes :: Parser ([SplType], SplRetType)
        parseFunTypes = label "function type definition" $ do
            args <- option [] $ many parseType
            _ <- symbol "->"
            retType <- (SplRetType <$> parseType) <|> pvoid
            return (args, retType)
            where
                pvoid = readWord "Void" *> return SplRetVoid

-- Type parser
parseType :: Parser SplType
parseType = (SplType <$> basicType)
        <|> (do
                _ <- symbol "("
                left <- parseType
                _ <- symbol ","
                right <- parseType
                _ <- symbol ")"
                return $ SplTypeTuple left right
             <?> "tuple")
        <|> ((SplTypeList <$> between (symbol "[") (symbol "]") parseType) 
                <?> "list")
        <|> (SplTypePlaceholder <$> identifier)

-- Read an expression
expr :: Parser SplExpr
expr = makeExprParser exprTerms operators

exprTerms :: Parser SplExpr
exprTerms = (SplIntLiteralExpr <$> int)
        <|> (SplBooleanLiteralExpr <$> bool)
        <|> (SplCharLiteralExpr <$> character)
        <|> (between (symbol "(") (symbol ")") expr <?> "Subexpression")

-- binary operators
operators :: [[Operator Parser SplExpr]]
operators = [
        [
            Prefix (SplUnaryExpr SplOperatorNegate <$ symbol "-"),
            Prefix (SplUnaryExpr SplOperatorInvert <$ symbol "~")
        ],
        [
            InfixL (SplBinaryExpr SplOperatorMultiply <$ symbol "*"),
            InfixL (SplBinaryExpr SplOperatorDivide <$ symbol "/"),
            InfixL (SplBinaryExpr SplOperatorModulus <$ symbol "%")
        ],
        [
            InfixL (SplBinaryExpr SplOperatorAdd <$ symbol "+"),
            InfixL (SplBinaryExpr SplOperatorSubtract <$ symbol "-")
        ],
        [
            InfixL (SplBinaryExpr SplOperatorLessEqual <$ readOperator "<="),
            InfixL (SplBinaryExpr SplOperatorLess <$ readOperator "<"),
            InfixL (SplBinaryExpr SplOperatorEqual <$ readOperator "=="),
            InfixL (SplBinaryExpr SplOperatorGreaterEqual <$ readOperator ">="),
            InfixL (SplBinaryExpr SplOperatorGreater <$ readOperator ">"),
            InfixL (SplBinaryExpr SplOperatorNotEqual <$ readOperator "!=")
        ],
        [
            InfixL (SplBinaryExpr SplOperatorAnd <$ readOperator "&&")
        ],
        [
            InfixL (SplBinaryExpr SplOperatorOr <$ readOperator "||")
        ],
        [
            InfixR (SplBinaryExpr SplOperatorCons <$ symbol ":")
        ]
    ]
    where
        readOperator :: String -> Parser ()
        readOperator s = void (string s) <* sc

stmt :: Parser SplStmt
stmt = (readWord "return" *> (SplReturnStmt <$> expr) <* symbol ";")
    <|> try (SplAssignmentStmt <$> identifier <*> field <* (symbol "=") <*> expr)

field :: Parser SplField
field = return SplFieldNone


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
int = (L.signed (void $ string "") (lexeme L.integer)) <?> "Integer literal"

-- Makes sure we read a bare word, useful for keywords etc.
readWord :: String -> Parser String
readWord w = do
    string w *> notFollowedBy alphaNumChar *> sc
    return w

-- Reserved words
reserved :: [String]
reserved = ["if", "else", "while", "return", "True", "False", "var"]

-- Parse an identifier
identifier :: Parser String
identifier = (lexeme . try) (parser >>= check)
    where
        -- (:) adds the parsed first letter to the list parsed by the many
        parser = (:) <$> letterChar <*> many (char '_' <|> alphaNumChar) <?> "identifier"
        check :: String -> Parser String
        check x = if x `elem` reserved
                    then fail $ show x ++ " is a reserved keyword"
                    else return x

-- Parse boolean literals
bool :: Parser Bool
bool = do
    w <- (readWord "True" <|> readWord "False") <?> "Boolean literal"
    return (w == "True")

-- Parse character literals
character :: Parser Char
character = label "Character literal" $ lexeme (char '\'' *> L.charLiteral <* char '\'')

-- read a basic type
basicType :: Parser SplBasicType
basicType = do
    t <- (readWord "Int" <|> readWord "Bool" <|> readWord "Char") <?> "type of either Int, Bool or Char"
    case t of
        "Int" -> return SplInt
        "Bool" -> return SplBool
        "Char" -> return SplChar
        _ -> fail "(Error in parser): Unexpected type"
