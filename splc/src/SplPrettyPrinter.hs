module SplPrettyPrinter where

import Text.PrettyPrint

import SplAST

pprint :: Spl -> String
pprint = render . printSpl

printSpl :: Spl -> Doc
printSpl (Spl []) = empty
printSpl (Spl (SplDeclVar var : xs)) = printVarDecl var <> semi $+$ printSpl (Spl xs)
printSpl (Spl (SplDeclFun name args argTypes retType varDecls stmts : xs)) 
     = text name 
    <> parens (printArgs args) 
    <+> (printTypeDecl argTypes retType)
    <+> lbrace $+$ (nest 4 $ printVarDecls varDecls $+$ printStmts stmts) $+$ rbrace
    $+$ printSpl (Spl xs)
    where
        printArgs args_ = commaSep (map text args_)

        printTypeDecl [] (SplRetType SplTypeUnknown) = empty
        printTypeDecl [] (SplRetVoid) = text ":: Void"
        printTypeDecl [] (SplRetType t) = text "::" <+> printType t
        printTypeDecl a b = text "::" <+> printTypeDecl' a b

        printTypeDecl' [] SplRetVoid = text "-> Void"
        printTypeDecl' [] (SplRetType t) = text "->" <+> printType t
        printTypeDecl' (t:ts) b = printType t <+> printTypeDecl' ts b
        
        printVarDecls [] = empty
        printVarDecls (v:vs) = printVarDecl v <> semi $+$ printVarDecls vs

printVarDecl :: SplVarDecl -> Doc
printVarDecl (SplVarDecl typedef name expr) =
    printType' typedef <+> text name <+> equals <+> printExpr expr
    where
        printType' SplTypeUnknown = text "var"
        printType' a = printType a

printStmts :: [SplStmt] -> Doc
printStmts [] = empty
printStmts (x:xs) = printStmt x $+$ printStmts xs

printStmt :: SplStmt -> Doc
printStmt (SplReturnVoidStmt) = text "return" <> semi
printStmt (SplReturnStmt expr) = text "return" <+> printExpr expr <> semi
printStmt (SplFuncCallStmt name args) = text name <> parens (commaSep (map printExpr args)) <> semi
printStmt (SplAssignmentStmt name field expr) =
    text name <> printField field <+> equals <+> printExpr expr <> semi
printStmt (SplWhileStmt cond stmts) = 
    text "while" <+> parens (printExpr cond) <+> lbrace $+$ (nest 4 $ printStmts stmts) $+$ rbrace
printStmt (SplIfStmt cond stmts []) =
    text "if" <+> parens (printExpr cond) <+> lbrace $+$ (nest 4 $ printStmts stmts) $+$ rbrace
printStmt (SplIfStmt cond thenStmts elseStmts) =
    text "if" <+> parens (printExpr cond) <+> lbrace $+$ (nest 4 $ printStmts thenStmts) $+$ rbrace
        <+> text "else" <+> lbrace $+$ (nest 4 $ printStmts elseStmts) $+$ rbrace

printType :: SplType -> Doc
printType (SplType basicType) = printBasicType basicType
printType (SplTypeTuple a b) = parens $ printType a <> comma <+> printType b
printType (SplTypeList a) = brackets $ printType a
printType (SplTypePlaceholder a) = text a
printType (SplTypeUnknown) = error "tried printing an unknown type"

printBasicType :: SplBasicType -> Doc
printBasicType SplInt = text "Int"
printBasicType SplBool = text "Bool"
printBasicType SplChar = text "Char"

printExpr :: SplExpr -> Doc
printExpr (SplIdentifierExpr name field) = text name <> printField field
printExpr (SplBinaryExpr op a b) = 
    maybeParensPrint a <+> printBinaryOperator op <+> maybeParensPrint b
printExpr (SplUnaryExpr op a) = printUnaryOperator op <> maybeParensPrint a
printExpr (SplIntLiteralExpr i) = integer i
printExpr (SplCharLiteralExpr c) = quotes $ char c
printExpr (SplBooleanLiteralExpr True) = text "True"
printExpr (SplBooleanLiteralExpr False) = text "False"
printExpr (SplFuncCallExpr name args) = text name <> parens (commaSep $ map printExpr args)
printExpr SplEmptyListExpr = text "[]"
printExpr (SplTupleExpr a b) = parens (printExpr a <> comma <+> printExpr b)

maybeParensPrint :: SplExpr -> Doc
maybeParensPrint e@(SplBinaryExpr SplOperatorCons _ _) = printExpr e
maybeParensPrint e@(SplBinaryExpr _ _ _) = parens (printExpr e)
maybeParensPrint e = printExpr e

printField :: SplField -> Doc
printField SplFieldNone = empty
printField (SplFieldHd f) = text ".hd" <> printField f
printField (SplFieldTl f) = text ".tl" <> printField f
printField (SplFieldFst f) = text ".fst" <> printField f
printField (SplFieldSnd f) = text ".snd" <> printField f

printUnaryOperator :: SplUnaryOperator -> Doc
printUnaryOperator SplOperatorInvert = text "!"
printUnaryOperator SplOperatorNegate = text "-"

printBinaryOperator :: SplBinaryOperator -> Doc
printBinaryOperator t = text (show t)

-- helper function, joins [a, b, .., c] into "a, b, .., c"
commaSep :: [Doc] -> Doc
commaSep = sep . punctuate comma
