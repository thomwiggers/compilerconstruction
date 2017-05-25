module SplParser where

import Control.Monad (void, when)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import SplAST


spl :: Parser Spl
spl = Spl <$> (sc *> parseDecls <* eof)

parseDecls :: Parser [SplDecl]
parseDecls = some $
        (try (SplDeclVar <$> varDecl) <?> "Variable definition")
    <|> (try funDecl <?> "Function definition")

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
    when (name == "print" || name == "printInt" || name == "isEmpty") $
        fail $ name ++ " is a reserved function name"

    args <- parens $ identifier `sepBy` symbol ","
    (argTypes, retType) <-
        option ([], SplRetType SplTypeUnknown)
            $ string "::" *> sc *> parseFunTypes

    -- check if num args matches num types if given
    when (not (null argTypes) && length argTypes /= length args) $
        fail "Number of arguments and types does not match"
    _ <- symbol "{"
    varDecls <- many (try varDecl) -- VarDecl*
    stmts <- some stmt  -- Stmt+
    _ <- symbol "}" <?> "end of function"
    return $ SplDeclFun name args argTypes retType varDecls stmts
    where
        parseFunTypes :: Parser ([SplType], SplRetType)
        parseFunTypes = label "function type definition" $ do
            args <- option [] $ try $ many parseType <* symbol "->"
            retType <- pvoid <|> (SplRetType <$> parseType)
            return (args, retType)
            where
                pvoid = (readWord "Void" *> return SplRetVoid) <?> "Void"

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
        <|> (string "[]" *> sc *> pure SplEmptyListExpr)
        <|> (SplCharLiteralExpr <$> character)
        -- Parse tuples or parentheses
        -- If we use try, runtime explodes
        <|> (do
                left <- symbol "(" *> expr
                c <- symbol ")" <|> symbol ","
                if c == ")"
                    then return left
                    else SplTupleExpr left <$> expr <* symbol ")"
            )
        -- parse identifiers or function calls
        -- Again, try is slow
        <|> (do
                name <- identifier
                c <- option "NONE" (lookAhead (symbol "." <|> symbol "("))
                case c of
                    "NONE" -> return $ SplIdentifierExpr name SplFieldNone
                    "(" -> SplFuncCallExpr name <$> parens (expr `sepBy` symbol ",")
                    "." -> SplIdentifierExpr name <$> field
                    _ -> fail "This should never happen (in exprTerms)"
            )

-- binary operators
operators :: [[Operator Parser SplExpr]]
operators = [
        [
            Prefix (manyUnary $ SplUnaryExpr SplOperatorNegate <$ symbol "-"),
            Prefix (manyUnary $ SplUnaryExpr SplOperatorInvert <$ symbol "!")
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
        manyUnary :: Parser (SplExpr -> SplExpr) -> Parser (SplExpr -> SplExpr)
        manyUnary x = foldr1 (.) <$> some x
        readOperator :: String -> Parser ()
        readOperator s = void (string s) <* sc

stmt :: Parser SplStmt
stmt = (readWord "return" *> (
                (SplReturnStmt <$> expr <* symbol ";")
            <|> (SplReturnVoidStmt <$ symbol ";")
        ))
    <|> (readWord "if" *> (SplIfStmt <$> parens expr <*> braces (many stmt)
            <*> option [] (readWord "else" *> braces (many stmt))))
    <|> (readWord "while" *> (SplWhileStmt <$> parens expr <*> braces (many stmt)))
    -- parse assignments or function calls
    -- Using try to handle the overlapping prefixes (identifiers) is slow
    <|> (do
            name <- identifier
            c <- option "NONE" (lookAhead (symbol "." <|> symbol "("))
            case c of
                "NONE" -> SplAssignmentStmt name SplFieldNone <$> parseAssignmentTail
                "(" -> SplFuncCallStmt name <$> parens (expr `sepBy` symbol ",") <* symbol ";"
                "." -> SplAssignmentStmt name <$> field <*> parseAssignmentTail
                _ -> fail "This should never happen (in exprTerms)"
        )
    where
        parseAssignmentTail = symbol "=" *> expr <* symbol ";"

field :: Parser SplField
field = char '.' *> (
            readKw "hd" *> (SplFieldHd <$> field)
        <|> readKw "tl" *> (SplFieldTl <$> field)
        <|> readKw "fst" *> (SplFieldFst <$> field)
        <|> readKw "snd" *> (SplFieldSnd <$> field)
    ) <* sc
    <|> return SplFieldNone
    where
        readKw :: String -> Parser ()
        readKw w = string w *> notFollowedBy alphaNumChar

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
int = L.signed (void $ string "") (lexeme L.integer) <?> "Integer literal"

-- Between braces
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Makes sure we read a bare word, useful for keywords etc.
readWord :: String -> Parser String
readWord w = do
    try (string w *> notFollowedBy alphaNumChar *> sc)
    return w

-- Reserved words
reserved :: [String]
reserved = ["if", "else", "while", "return", "True", "False", "var", "Void", "Int", "Bool"]

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
