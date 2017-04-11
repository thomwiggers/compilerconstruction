Spl 
[
  SplDeclFun "fun" [] [] (SplRetType SplTypeUnknown)
    [SplVarDecl (SplType SplInt) "a" (SplIntLiteralExpr 1)] 
    [SplAssignmentStmt "a" SplFieldNone 
      (SplBinaryExpr SplOperatorAdd 
        (SplIdentifierExpr "a" SplFieldNone) 
        (SplIntLiteralExpr 1)
    ]
]
