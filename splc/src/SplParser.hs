module SplParser where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

-- SPL = Decl+
data Spl = Spl [SplDecl]
    deriving (Show, Eq)

-- Decl = VarDecl | FunDecl
data SplDecl = SplDeclVar (SplVarDecl) -- nested because we need vardecl seperately
                         -- id ( fargs ) [ :: type ] { VarDecl* Stmt+ }
             | SplFunDecl String [String] [SplRetType] SplRetType [SplVarDecl] [SplStmt]
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
             | SplBinaryExpr SplExpr SplBinaryOperator SplExpr
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

varDecl :: Parser SplVarDecl
varDecl = do
    t <- ((readWord "var" *> return SplTypeUnknown) <|> (SplType <$> basicType)) <?> "var or a type"
    i <- identifier
    _ <- symbol "="
    e <- expr
    _ <- symbol ";"
    return $ SplVarDecl t i e

funDecl :: Parser SplDecl
funDecl = fail "Not implemented"

-- Read an expression
expr :: Parser SplExpr
expr = (SplIntLiteralExpr <$> int)

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

-- read a basic type
basicType :: Parser SplBasicType
basicType = do
    t <- (readWord "Int" <|> readWord "Bool" <|> readWord "Char") <?> "type of either Int, Bool or Char"
    case t of
        "Int" -> return SplInt
        "Bool" -> return SplBool
        "Char" -> return SplChar
        _ -> fail "(Error in parser): Unexpected type"

-- Parse Unary operators
unaryOperator :: Parser SplUnaryOperator
unaryOperator = do
    t <- (readWord "-" <|> readWord "!") <?> "Unary operator"
    case t of
        "-" -> return SplOperatorNegate
        "!" -> return SplOperatorInvert
        _ -> fail "unknown unary operator (error in parser)"
