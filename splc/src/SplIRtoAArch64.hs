module SplIRtoAArch64 where

import SplAST (SplBinaryOperator(..), SplUnaryOperator(..))
import SplIR
import SplAArch64
import Control.Monad.State
import Data.Char (ord)

newtype AArch64State = AArch64State {
        stackPtr  :: Int
    }

emptyAArch64State :: AArch64State
emptyAArch64State = AArch64State {stackPtr = 0}

type IRtoAArch64State = State AArch64State [AArch64Instruction]

compileToAArch64 :: SplIR -> [AArch64Instruction]
compileToAArch64 ir = evalState (programToAArch64 ir) emptyAArch64State

programToAArch64 :: SplIR -> IRtoAArch64State
programToAArch64 ir = concat <$> mapM toAArch64 ir

toAArch64 :: SplInstruction -> IRtoAArch64State
-- binary operations
toAArch64 (SplBinaryOperation SplOperatorAdd (Reg rd) (Reg rn) (Reg rm))
    = return [ADD (PR rd) (PR rn) (PR rm)]
toAArch64 (SplBinaryOperation SplOperatorSubtract (Reg rd) (Reg rn) (Reg rm))
    = return [SUB (PR rd) (PR rn) (PR rm)]
toAArch64 (SplBinaryOperation SplOperatorMultiply (Reg rd) (Reg rn) (Reg rm))
    = return [MUL (PR rd) (PR rn) (PR rm)]
toAArch64 (SplBinaryOperation SplOperatorDivide (Reg rd) (Reg rn) (Reg rm))
    = return [SDIV (PR rd) (PR rn) (PR rm)]
-- FIXME rest of binops

-- unary operations:
toAArch64 (SplUnaryOperation SplOperatorInvert (Reg rd) (Reg rm))
    = return [XOR (PR rd) (PR rm) (Imm 1)]
toAArch64 (SplUnaryOperation SplOperatorNegate (Reg rd) (Reg rm))
    = return [NEG (PR rd) (PR rm)]

-- move
toAArch64 (SplMov (Reg rd) (Reg rn)) = return [MOV (PR rd) (PR rn)]
toAArch64 (SplMovImm (Reg rd) (SplImmInt i)) = return [MOV (PR rd) (Imm (fromInteger i))]
toAArch64 (SplMovImm (Reg rd) (SplImmBool b))
    = return [MOV (PR rd) (Imm $ if b then 1 else 0)]
toAArch64 (SplMovImm (Reg rd) (SplImmChar c))
    = return [MOV (PR rd) (Imm (ord c))]

-- return
toAArch64 (SplRet Nothing) = return [RET]
toAArch64 (SplRet (Just (Reg r)))
    = return [MOV (X 0) (PR r), RET]

-- functions
toAArch64 (SplFunction label [] ir) = do
    functionCode <- concat <$> mapM toAArch64 ir
    return [BasicBlock $ Label label : functionCode ++ [RET]]

-- if
toAArch64 (SplIf label (Reg cond) thenIR elseIR) = do
    let elseLabel = label ++ "else"
    let endifLabel = label ++ "end"
    let ifStart = [Comment $ "if " ++ label,
                   CMP (PR cond) (Imm 0),
                   BranchConditional Equal elseLabel]

    thenCode <- concat <$> mapM toAArch64 thenIR

    elseCode <- if not (null elseIR)
        then (do
            elseCode' <- concat <$> mapM toAArch64 elseIR
            return $ [BranchAlways endifLabel,
                      Label elseLabel] ++
                      elseCode' ++
                      [Label endifLabel]
        )
        else return [Label elseLabel]

    return [BasicBlock ifStart, BasicBlock thenCode, BasicBlock elseCode]

-- while
toAArch64 (SplWhile label (condIR, Reg condReg) bodyIR) = do
    let endLabel = label ++ "end"
    condCode <- concat <$> mapM toAArch64 condIR
    let condBlock = BasicBlock $ [Label label] ++
                     condCode ++
                     [CMP (PR condReg) (Imm 0),
                     BranchConditional Equal endLabel]

    bodyCode <- concat <$> mapM toAArch64 bodyIR
    let bodyBlock = BasicBlock $ bodyCode ++
                                [BranchAlways label,
                                 Label endLabel]

    return [condBlock, bodyBlock]
