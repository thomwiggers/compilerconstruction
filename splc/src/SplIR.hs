module SplIR where

import SplAST

-- FIXME split declarations and functions as separate blocks
type SplIR = [SplInstruction]

data SplPseudoRegister
    -- simple register
    = Reg String
    -- ex: TupleLeft (TupleRight (Reg mijntuple))
    | TupleFst SplPseudoRegister
    | TupleSnd SplPseudoRegister
    -- list value and pointer to tail
    | ListHd SplPseudoRegister
    | ListTl SplPseudoRegister
    -- ex: ListHd (TupleFst (Reg mijnTuplemetlinkseenlijst))
    -- for tail empty
    | EmptyList
    deriving (Show, Eq)

type SplLabel = String

data SplImm = SplImmInt Integer | SplImmBool Bool | SplImmChar Char
    deriving Show

data SplInstruction
    = SplFunction SplLabel [SplPseudoRegister] SplIR
    -- if   unique   cond              Then  Else
    | SplIf SplLabel SplPseudoRegister SplIR SplIR
    -- while   unique   cond  body
    | SplWhile SplLabel (SplIR, SplPseudoRegister) SplIR
    -- binop             op                dest              a                 b
    | SplBinaryOperation SplBinaryOperator SplPseudoRegister SplPseudoRegister SplPseudoRegister
    -- unary op         op               dest              src
    | SplUnaryOperation SplUnaryOperator SplPseudoRegister SplPseudoRegister
    | SplJumpTarget SplLabel
    | SplRet (Maybe SplPseudoRegister)
    | SplJump SplLabel
    | SplJumpIf SplPseudoRegister SplLabel
    | SplJumpIfNot SplPseudoRegister SplLabel
    -- mov   dest              src
    | SplMov SplPseudoRegister SplPseudoRegister
    | SplMovImm SplPseudoRegister SplImm
    | SplCall SplLabel (Maybe SplPseudoRegister) [SplPseudoRegister]
    deriving (Show)

isFunction :: SplInstruction -> Bool
isFunction SplFunction{} = True
isFunction _             = False
