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
    -- while   unique   cond                       body
    | SplWhile SplLabel (SplIR, SplPseudoRegister) SplIR
    -- binop             op                dest              a                 b
    | SplBinaryOperation SplBinaryOperator SplPseudoRegister SplPseudoRegister SplPseudoRegister
    -- unary op         op               dest              src
    | SplUnaryOperation SplUnaryOperator SplPseudoRegister SplPseudoRegister
    | SplRet (Maybe SplPseudoRegister)
    -- mov   dest              src
    | SplMov SplPseudoRegister SplPseudoRegister
    | SplMovImm SplPseudoRegister SplImm
    | SplCall SplLabel (Maybe SplPseudoRegister) [SplPseudoRegister]
    -- make a new tuple     name           left             right
    | SplTupleConstr SplPseudoRegister SplPseudoRegister SplPseudoRegister

(+++) :: String -> String -> String
(+++) a b = a ++ " " ++ b

instance Show SplInstruction where
    show (SplFunction label args ir) =
        "SplFunction " ++ label +++ show args ++ "\n{\n" ++ printList ir ++ "\n}"
    show (SplIf label cond thenIR elseIR) =
        "SplIf " ++ label ++ " (" ++ show cond ++ ") {\n" ++ printList thenIR ++
        "\n}\n" ++ " else {\n" ++ printList elseIR ++ "\n}"
    show (SplWhile label (condIR, condReg) blockIR) =
        "SplWhile" +++ label +++ "{\n" ++ printList condIR ++ "\n} -> " ++ show condReg ++ " == true then \n{\n" ++
        printList blockIR ++ "\n}"
    show (SplBinaryOperation op r1 r2 r3) =
        "SplBinaryOperation" +++ show op +++ show r1 +++ show r2 +++ show r3
    show (SplUnaryOperation op r1 r2) = "SplUnaryOperation" +++ show op +++ show r1 +++ show r2
    show (SplRet reg) = "SplRet" +++ show reg
    show (SplMov r1 r2) = "SplMov" +++ show r1 +++ show r2
    show (SplMovImm r1 i) = "SplMovImm" +++ show r1 +++ show i
    show (SplCall label res args) = "SplCall" +++ label +++ show args +++ "->" +++ show res
    show (SplTupleConstr r1 l r) = show r1 +++ "=" +++ "(" ++ show l ++ ", " ++ show r ++ ")"

isFunction :: SplInstruction -> Bool
isFunction SplFunction{} = True
isFunction _             = False

printList :: (Show a) => [a] -> String
printList [] = []
printList (x:xs) = show x ++ "\n" ++ printList xs
