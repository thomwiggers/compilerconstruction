module SplAArch64 (module SplAArch64) where

import           Prelude hiding (EQ, GT, LT)

type Offset = Int
type Size = Int
type Label = String

-- see http://infocenter.arm.com/help/topic/com.arm.doc.ihi0055b/IHI0055B_aapcs64.pdf
            -- 0-28     29   30   '31'
data Register = X Int | FP | LR | SP | PR String | Imm Int
    deriving (Eq, Ord)

-- callee-saved: r19-r29, sp

data Address = Address Register Offset
    deriving Eq

wordLength :: Int
wordLength = 8

instance Show Address where
    show (Address r o) = "[" ++ show r ++ ", #" ++ show o ++ "]"

data Condition = Equal | NotEqual | Greater | GreaterEqual | Less | LessEqual
    deriving (Show, Eq)

asmCond :: Condition -> String
asmCond Equal        = "EQ"
asmCond NotEqual     = "NE"
asmCond Greater      = "GT"
asmCond GreaterEqual = "GE"
asmCond Less         = "LT"
asmCond LessEqual    = "LE"

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
    | EOR Register Register Register
    | CMP          Register Register
    | CSET Register Condition
    | MOV Register Register
    | MOVK Register Register
    | BranchConditional Condition Label
    | BranchAlways Label
    | BranchLink Label
    | LDR Register Address      -- ldr x0, [sp, #16]
    | STR Register Address      -- str x0, [sp, #12]
    | ADRP Register Label
    | AddNamedOffset Register Register Label
    | RET
    | NOP
    | Comment String
    | Label Label
    | BasicBlock [AArch64Instruction]
    deriving Eq

(+++) :: (Show b) => String -> b -> String
(+++) a b = a ++ " " ++ show b

-- operatornaam is sws ellende: moet symbol zijn, komma mag niet
(++<) :: (Show b) => String -> b -> String
(++<) a b = a ++ ", " ++ show b

instance Show AArch64Instruction where
    show (ADD r a b)           = "ADD" +++ r ++< a ++< b
    show (MUL r a b)           = "MUL" +++ r ++< a ++< b
    show (SDIV r a b)          = "SDIV" +++ r ++< a ++< b
    show (SUB r a b)           = "SUB" +++ r ++< a ++< b
    show (NEG r a)             = "NEG" +++ r ++< a
    show (MSUB r a b c)        = "MSUB" +++ r ++< a ++< b ++< c
    show (AND r a b)           = "AND"  +++ r ++< a ++< b
    show (OR r a b)            = "OR" +++ r ++< a ++< b
    show (EOR r a b)           = "EOR" +++ r ++< a ++< b
    show (CMP a b)             = "CMP" +++ a ++< b
    show (CSET a c)            = "CSET" +++ a ++ ", " ++ asmCond c
    show (MOV r a)             = "MOV" +++ r ++< a
    show (MOVK r a)            = "MOVK" +++ r ++< a
    show (BranchConditional cond label)
                               = "B." ++ asmCond cond ++ " " ++ label
    show (BranchAlways label)  = "B " ++ label
    show (BranchLink label)    = "BL " ++ label
    show (LDR r a)             = "LDR" +++ r ++< a
    show (STR r a)             = "STR" +++ r ++< a
    show (ADRP r n)            = "ADRP" +++ r ++ ", " ++ n
    show (AddNamedOffset r a n) = "ADD" +++ r ++< a ++ ", :lo12:" ++ n
    show RET                   = "RET"
    show NOP                   = "NOP"
    show (Comment str)         = "// " ++ str
    show (Label name)          = name ++ ":"
    show (BasicBlock instrs)   = foldr (\a b -> show a ++ "\n" ++ b) "" instrs


functionName :: String -> String
functionName "main" = "_splmain"
functionName a = a
