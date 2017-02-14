module SplParser where

import Control.Monad (void, when)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

-- SPL = Decl+
data Spl = Spl [SplDecl]
    deriving (Show, Eq)

-- Decl = VarDecl | FunDecl
data SplDecl = SplDeclVar (SplVarDecl) -- nested because we need vardecl seperately
                         -- id ( fargs ) [ :: type ] { VarDecl* Stmt+ }
             | SplDeclFun String [String] [SplType] SplRetType [SplVarDecl] [SplStmt]
    deriving (Show, Eq)

                -- (var | Type) id = expr ;
data SplVarDecl = SplVarDecl SplType String SplExpr
    deriving (Show, Eq)

data SplRetType = SplRetType SplType
                | SplRetVoid
    deriving (Show, Eq)

data SplType = SplType SplBasicType
             | SplTypeUnknown
             | SplTypeTuple SplType SplType
             | SplTypeList (SplType)
             | SplTypePlaceholder String
    deriving (Show, Eq)

data SplBasicType = SplBool | SplInt | SplChar
    deriving (Show, Eq)

data SplStmt = SplIfStmt SplExpr [SplStmt] [SplStmt]
             | SplWhileStmt SplExpr [SplStmt]
               -- id[.field] = expr
             | SplAssignmentStmt String SplField SplExpr
               -- id(args)
             | SplFuncCallStmt String [SplExpr]
               -- return Expr
             | SplReturnStmt SplExpr
    deriving (Show, Eq)

-- Field = [ Field ['.'] (hd | tl | fst | snd) ]
data SplField = SplFieldHd SplField
              | SplFieldTl SplField
              | SplFieldFst SplField
              | SplFieldSnd SplField
              | SplFieldNone
    deriving (Show, Eq)

               -- id [.field]
data SplExpr = SplIdentifierStmt String SplField
             | SplBinaryExpr SplBinaryOperator SplExpr SplExpr
             | SplUnaryExpr SplUnaryOperator SplExpr
             | SplIntLiteralExpr Integer
             | SplCharLiteralExpr Char
             | SplBooleanLiteralExpr Bool
             | SplNestedExpr SplExpr
             | SplFuncCallExpr String [SplExpr]
             | SplEmptyListExpr
             | SplTupleExpr SplExpr SplExpr
    deriving (Show, Eq)

data SplBinaryOperator = SplOperatorAdd
                       | SplOperatorSubtract
                       | SplOperatorMultiply
                       | SplOperatorDivide
                       | SplOperatorModulus
                       | SplOperatorLess
                       | SplOperatorLessEqual
                       | SplOperatorEqual
                       | SplOperatorGreaterEqual
                       | SplOperatorGreater
                       | SplOperatorNotEqual
                       | SplOperatorAnd
                       | SplOperatorOr
                       | SplOperatorCons
    deriving (Eq, Show)

data SplUnaryOperator = SplOperatorInvert
                      | SplOperatorNegate
    deriving (Show, Eq)


spl :: Parser Spl
spl = Spl <$> parseDecls <* eof

parseDecls :: Parser [SplDecl]
parseDecls = some $
    ((SplDeclVar <$> varDecl) <?> "Variable definition")
    <|> (funDecl <?> "Function definition")

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
    (argTypes, retType) <- option ([], SplRetType SplTypeUnknown) $ string "::" *> sc *> parseFunTypes
    when (length argTypes /= length args) $ fail "Number of arguments and types does not match"
    let varDecls = []
    let stmts = [SplReturnStmt (SplIntLiteralExpr 1)]
    -- todo parse vardecl* statements+
    return $ SplDeclFun name args argTypes retType varDecls stmts
    where
        parseFunTypes :: Parser ([SplType], SplRetType)
        parseFunTypes = do
            args <- option [] $ (parseType `endBy` (string "->" *> sc))
            retType <- (SplRetType <$> parseType) <|> pvoid
            return (args, retType)
        pvoid = readWord "Void" *> return SplRetVoid

-- Type parser
parseType :: Parser SplType
parseType = SplType <$> basicType

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

-- Parse boolean literals
bool :: Parser Bool
bool = do
    w <- (readWord "True" <|> readWord "False") <?> "Boolean literal"
    return (w == "True")

-- Parse character literals
character :: Parser Char
character = label "Character literal" $ between (char '\'') (char '\'') theChar
    where
    theChar = choice $ escapes ++ [anyChar]
    escapes :: [Parser Char]
    escapes = map themap [
        ("\\n", '\n'), ("\\t", '\t'), ("\\r", '\r'), ("\\'", '\''),
        ("\\\\", '\\')]
    themap :: (String, Char) -> Parser Char
    themap (s, c) = do
        _ <- string s
        return $ c

-- read a basic type
basicType :: Parser SplBasicType
basicType = do
    t <- (readWord "Int" <|> readWord "Bool" <|> readWord "Char") <?> "type of either Int, Bool or Char"
    case t of
        "Int" -> return SplInt
        "Bool" -> return SplBool
        "Char" -> return SplChar
        _ -> fail "(Error in parser): Unexpected type"
