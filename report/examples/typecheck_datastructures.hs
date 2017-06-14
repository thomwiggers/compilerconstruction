type Environment 
  = Map.Map String SplTypeR
type SplTypeCheckResult 
  = StateT Environment (Either String) SplTypeR

data SplSimpleTypeR 
  = SplTypeConst SplBasicType
  | SplTypeTupleR SplSimpleTypeR SplSimpleTypeR
  | SplTypeListR SplSimpleTypeR
  | SplVoid
  deriving (Show, Eq)

data SplTypeR 
  = SplSimple SplSimpleTypeR
  | SplTypeFunction [SplSimpleTypeR] SplSimpleTypeR
  deriving (Show, Eq)

class SplTypeChecker a where
  typeCheck :: a -> SplTypeCheckResult
