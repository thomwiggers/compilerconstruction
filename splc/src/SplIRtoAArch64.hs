module SplIRtoAArch64 where

import SplAArch64
import Control.Monad.Writer
import Control.Monad.State

newtype AArch64State = AArch64State {
        stackPtr  :: Int
    }

type IRtoAArch64State = WriterT [AArch64Instruction] (State AArch64State) ()
