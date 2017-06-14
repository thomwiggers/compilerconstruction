typeCheck (SplUnaryExpr op expr) = do
  exprType <- typeCheck expr >>= unsimple >>= disallowVoid
  case op of
    SplOperatorInvert -> if (exprType == SplTypeConst SplBool)
      then returnSimple exprType
      else fail "Cannot invert a non-Bool"
    SplOperatorNegate -> if (exprType == SplTypeConst SplInt)
      then returnSimple exprType
      else fail "Cannot negate a non-Int"
