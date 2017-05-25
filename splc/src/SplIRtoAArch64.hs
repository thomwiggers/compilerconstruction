module SplIRtoAArch64 where

import SplAST (SplBinaryOperator(..), SplUnaryOperator(..))
import SplIR
import SplAArch64
import Control.Monad.Writer
import Control.Monad.State

newtype AArch64State = AArch64State {
        stackPtr  :: Int
    }

type IRtoAArch64State = WriterT [AArch64Instruction] (State AArch64State) ()

out :: AArch64Instruction -> IRtoAArch64State
out x = tell [x]


programToAArch64 :: SplIR -> IRtoAArch64State
programToAArch64 ir = mapM_ toAArch64 ir


toAArch64 :: SplInstruction -> IRtoAArch64State
-- binary operations
toAArch64 (SplBinaryOperation SplOperatorAdd (Reg rd) (Reg rn) (Reg rm))
    = out $ ADD (PR rd) (PR rn) (PR rm)
toAArch64 (SplBinaryOperation SplOperatorSubtract (Reg rd) (Reg rn) (Reg rm))
    = out $ SUB (PR rd) (PR rn) (PR rm)
toAArch64 (SplBinaryOperation SplOperatorMultiply (Reg rd) (Reg rn) (Reg rm))
    = out $ MUL (PR rd) (PR rn) (PR rm)
toAArch64 (SplBinaryOperation SplOperatorDivide (Reg rd) (Reg rn) (Reg rm))
    = out $ SDIV (PR rd) (PR rn) (PR rm)
-- FIXME rest of binops

-- unary operations:
toAArch64 (SplUnaryOperation SplOperatorInvert (Reg rd) (Reg rm))
    = out $ XOR (PR rd) (PR rm) (Imm 1)
toAArch64 (SplUnaryOperation SplOperatorNegate (Reg rd) (Reg rm))
    = out $ NEG (PR rd) (PR rm)

-- functions
toAArch64 (SplFunction label _args ir) = do
    out $ Label label
    out $ Comment "FIXME args"
    mapM_ toAArch64 ir
    out RET
