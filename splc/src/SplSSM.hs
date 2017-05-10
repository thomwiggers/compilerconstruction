module SplSSM where

type Offset = Int
type Size = Int
type Label = String

data Register = PC | SP | MP | HP | RR | R5 | R6 | R7 | PR String
    deriving (Show, Eq)

data Syscall
    = PopPrintInt       -- 0
    | PopPrintChar      -- 1
    -- there are more, but we don't support them.
    deriving (Show, Eq)

data SSM
    = LDC Int   -- load a constant.
    | LDS Offset   -- load a value relative to the SP.
    | LDH Offset  -- load a value relative to the HP.
    | LDL Offset  -- load a constant value relative to the MP.
    | LDA Offset  -- load a value pointed to by the value on top of the stack.
    | LDMA Offset Size -- Loads Size values in the stack from the address at the (SP + offset)
    | LDMH Offset Size  -- loads Size values from the heap to the stack
    | LDML Offset Size  -- load multiple local from the MP + offset and push them on the stack.
    | LDMS Offset Size -- load multiple values from the (SP + offset) and push them on the stack
    | LDR Register -- load a register value.
    | LDRR Register Register -- load the second register to the first (mov)
    | LDSA Offset  -- load address of value relative to the SP.
    | LDLA Offset -- load address of value relative to the MP.
    | LDAA Offset -- Adds the offset to the value at SP
    | STS Offset  -- store a value relative to the SP, on the stack.
    | STH         -- store a value from the stack to the HP, pushes that address on the stack
    | STL Offset  -- store a value relative to the MP
    | STA Offset  -- store a value pointed to by a value on the stack
    | STR Register  -- store a value from the stack in a register.
    | ADJ Offset -- adjusts stack pointer
    | LINK Size  -- does link stuff
    | UNLINK     -- undo link
    | ADD        -- Adds up two stack values and replaces them by the result
    | SUB
    | MUL
    | DIV
    | MOD
    | NEG
    | AND
    | OR
    | XOR
    | NOT
    | CMP
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
    deriving (Show, Eq)
