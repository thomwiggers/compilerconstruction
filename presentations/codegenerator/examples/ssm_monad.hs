data SSMState = SSMState {
         stackPtr :: Int,
        globalMap :: Map.Map String (Offset, Scope),
        scopedMap :: Map.Map String (Offset, Scope)
    }

type IRtoSSMState = WriterT [SSM] (State SSMState) ()
