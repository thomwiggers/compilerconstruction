toSSM :: SplInstruction -> IRtoSSMState
toSSM (SplBinaryOperation op (Reg rd) (Reg r1) (Reg r2)) = do
    loadFromStack r1
    loadFromStack r2
    case op of
        SplOperatorAdd -> out ADD
        SplOperatorSubtract -> out SUB
        SplOperatorMultiply -> out MUL
        SplOperatorDivide -> out DIV
        SplOperatorModulus -> out MOD
        SplOperatorLess -> out SplSSM.LT
        SplOperatorLessEqual -> out LE
        SplOperatorEqual -> out CMP
        -- (more omitted for slide)
    decreaseStackPointer
    decreaseStackPointer
    modify $ pushOnStack rd 1
