module SplAArch64 where

import           Prelude hiding (EQ, GT, LT)

type Offset = Int
type Size = Int
type Label = String

data Register = X Int | FP | LR | SP | PR String | Imm Int
    deriving Eq

data Address = Address Register Offset
    deriving Eq

instance Show Address where
    show (Address r o) = "[" ++ show r ++ ", #" ++ show o ++ "]"

data Condition = Equal | NotEqual | Greater | GreaterEqual | Less | LessEqual
    deriving (Show, Eq)

instance Show Register where
    show (X a) = if a < 0 || a > 28
                    then fail "BAD REGISTER x" ++ show a
                    else "x" ++ show a
    show (PR s) = "PR '" ++ s ++ "'"
    show (Imm i) = "#" ++ show i
    show FP = "fp"
    show LR = "lr"
    show SP = "sp"

-- FIXME encode MOD using MSUB

data AArch64Instruction
    --    Rd       Rn       Rm        Ra
    = ADD Register Register Register
    | MUL Register Register Register
    | SDIV Register Register Register
    | SUB Register Register Register
    | NEG Register Register
    | MSUB Register Register Register Register -- Rd = Ra - Rn * Rm (note Ra is last register!)
    | AND Register Register Register
    | OR Register Register Register
    | XOR Register Register Register
    | CMP          Register Register
    | MOV Register Register
    | BranchConditional Condition Label
    | BranchAlways Label
    | BranchLink Label
    | LDR Register Address      -- ldr x0, [sp, #16]
    | STR Register Address      -- str x0, [sp, #12]
    | RET
    | NOP
    | Comment String
    | Label Label
    | BasicBlock [AArch64Instruction]
    deriving (Show, Eq)
