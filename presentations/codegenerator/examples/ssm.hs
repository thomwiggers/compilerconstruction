data SSM
    = LDC Int
    | LDS Offset
    | LDH Offset
    | LDL Offset
    | LDA Offset
    | LDMA Offset Size
    | LDMH Offset Size
    | LDML Offset Size
    | LDMS Offset Size
    | LDR Register
    | LDRR Register Register
    | LDSA Offset
    | LDLA Offset
    | LDAA Offset
    | STS Offset
    | STH
    | STL Offset
    | STA Offset
    | STR Register
    | ADJ Offset
    | LINK Size
    | UNLINK
    | ADD
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
    | BEQ Label
    | BNE Label
    | BLT Label
    | BGT Label
    | BLE Label
    | BGE Label
    | BRA Label
    | BSR Label
    | BRT Label
    | BRF Label
    | JSR
    | RET
    | HALT
    | TRAP Syscall
    derive Eq
