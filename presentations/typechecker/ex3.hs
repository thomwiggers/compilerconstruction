class SplTypeChecker a where
  typeCheck :: a -> SplTypeCheckResult

instance SplTypeChecker SplExpr where
  typeCheck (SplIntLiteralExpr _) 
             = return $ SplSimple $ SplTypeConst SplInt
