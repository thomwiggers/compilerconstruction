module SplAST where

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

