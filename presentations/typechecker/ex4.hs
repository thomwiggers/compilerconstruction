data SplTypeR 
  = SplSimple SplSimpleTypeR
  | SplTypeFunction [SplSimpleTypeR] SplSimpleTypeR

data SplSimpleTypeR 
  = SplTypeConst SplBasicType
  | SplTypeTupleR SplSimpleTypeR SplSimpleTypeR
  | SplTypeListR SplSimpleTypeR
  | SplTypeVar TVar
  | SplVoid
