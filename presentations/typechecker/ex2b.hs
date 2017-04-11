type SplTypeCheckResult 
  = StateT Environment 
           (Either String) 
           SplTypeR
