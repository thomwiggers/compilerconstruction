data SplInstruction
    = SplFunction SplLabel [SplPseudoRegister] SplIR
    | SplBinaryOperation SplBinaryOperator SplPseudoRegister
                         SplPseudoRegister SplPseudoRegister
    | SplUnaryOperation SplUnaryOperator SplPseudoRegister
                        SplPseudoRegister
    | SplJumpTarget SplLabel
    | SplRet SplPseudoRegister
    | SplJump SplLabel
    | SplJumpIf SplPseudoRegister SplLabel
    | SplJumpIfNot SplPseudoRegister SplLabel
    | SplMov SplPseudoRegister SplPseudoRegister
    | SplMovImm SplPseudoRegister SplImm
    | SplCall SplLabel [SplPseudoRegister]
    deriving Eq
