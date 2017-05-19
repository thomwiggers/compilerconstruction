module SplSSM where

import           Prelude hiding (EQ, GT, LT)

type Offset = Int
type Size = Int
type Label = String

data Register = PC | SP | MP | HP | RR | R5 | R6 | R7
    deriving (Show, Eq)

data Syscall
    = PopPrintInt       -- 0
    | PopPrintChar      -- 1
    -- there are more, but we don't support them.
    deriving Eq

instance Show Syscall where
    show PopPrintInt  = "0"
    show PopPrintChar = "1"

data SSM
    = LDC Int                   -- load a constant.
    | LDS Offset                -- load a value relative to the SP.
    | LDH Offset                -- load a value relative to the HP.
    | LDL Offset                -- load a constant value relative to the MP.
    | LDA Offset                -- load a value pointed to by the value on top of the stack.
    | LDMA Offset Size          -- Loads Size values in the stack from the address at the (SP + offset)
    | LDMH Offset Size          -- loads Size values from the heap to the stack
    | LDML Offset Size          -- load multiple local from the MP + offset and push them on the stack.
    | LDMS Offset Size          -- load multiple values from the (SP + offset) and push them on the stack
    | LDR Register              -- load a register value.
    | LDRR Register Register    -- load the second register to the first (mov)
    | LDSA Offset               -- load address of value relative to the SP.
    | LDLA Offset               -- load address of value relative to the MP.
    | LDAA Offset               -- Adds the offset to the value at SP
    | STS Offset                -- store a value relative to the SP, on the stack.
    | STH                       -- store a value from the stack to the HP, pushes that address on the stack
    | STMH Size                 -- Store size words from the stack onto the heap
    | STL Offset                -- store a value relative to the MP
    | STA Offset                -- store a value pointed to by a address on the stack
    | STR Register              -- store a value from the stack in a register.
    | AJS Offset                -- adjusts stack pointer
    | LINK Size                 -- does link stuff
    | UNLINK                    -- undo link
    | ADD                       -- Adds up two stack values and replaces them by the result
    | SUB
    | MUL
    | DIV
    | MOD
    | NEG
    | AND
    | OR
    | XOR
    | NOT
    | EQ
    | NE
    | LT
    | GT
    | LE
    | GE
    | BEQ Label     -- Branch if equals
    | BNE Label
    | BLT Label
    | BGT Label
    | BLE Label
    | BGE Label
    | BRA Label     -- branch always
    | BSR Label     -- branch subroutine, pushes PC
    | BRT Label     -- branch if top of stack is true
    | BRF Label     -- branch if top of stack is False
    | JSR           -- branch based on value on stack
    | RET
    | HALT
    | TRAP Syscall  -- Call system call
    | Label Label
    | Comment String
    deriving Eq

showSSM :: [SSM] -> String
showSSM []     = ""
showSSM (x:xs) = show x ++ "\n" ++ showSSM xs

instance Show SSM where
    show (LDC i)            = "LDC " ++ show i
    show (LDS offset)       = "LDS " ++ show offset
    show (LDH offset)       = "LDH " ++ show offset
    show (LDL offset)       = "LDL " ++ show offset
    show (LDA offset)       = "LDA " ++ show offset
    show (LDMA offset size) = "LDMA " ++ show offset ++ " " ++ show size
    show (LDMH offset size) = "LDMH " ++ show offset ++ " " ++ show size
    show (LDML offset size) = "LDML " ++ show offset ++ " " ++ show size
    show (LDMS offset size) = "LDMS " ++ show offset ++ " " ++ show size
    show (LDR register)     = "LDR " ++ show register
    show (LDRR rd rs)       = "LDRR " ++ show rd ++ " " ++ show rs
    show (LDSA offset)      = "LDSA " ++ show offset
    show (LDLA offset)      = "LDLA " ++ show offset
    show (LDAA offset)      = "LDAA " ++ show offset
    show (STS offset)       = "STS " ++ show offset
    show STH                = "STH"
    show (STMH size)        = "STMH " ++ show size
    show (STL offset)       = "STL " ++ show offset
    show (STA offset)       = "STA " ++ show offset
    show (STR register)     = "STR " ++ show register
    show (AJS offset)       = "AJS " ++ show offset
    show (LINK size)        = "LINK " ++ show size
    show UNLINK             = "UNLINK"
    show ADD                = "ADD"
    show SUB                = "SUB"
    show MUL                = "MUL"
    show DIV                = "DIV"
    show MOD                = "MOD"
    show NEG                = "NEG"
    show AND                = "AND"
    show OR                 = "OR"
    show XOR                = "XOR"
    show NOT                = "NOT"
    show EQ                 = "EQ"
    show NE                 = "NE"
    show LT                 = "LT"
    show GT                 = "GT"
    show LE                 = "LE"
    show GE                 = "GE"
    show (BEQ label)        = "BEQ " ++ label
    show (BNE label)        = "BNE " ++ label
    show (BLT label)        = "BLT " ++ label
    show (BGT label)        = "BGT " ++ label
    show (BLE label)        = "BLE " ++ label
    show (BGE label)        = "BGE " ++ label
    show (BRA label)        = "BRA " ++ label
    show (BSR label)        = "BSR " ++ label
    show (BRT label)        = "BRT " ++ label
    show (BRF label)        = "BRF " ++ label
    show JSR                = "JSR"
    show RET                = "RET"
    show HALT               = "HALT"
    show (TRAP syscall)     = "TRAP " ++ show syscall
    show (Label label)      = label ++ ":"
    show (Comment string)   = "; " ++ string
