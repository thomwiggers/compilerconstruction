{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module SplIRtoSSM where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Char            (ord)
import           Data.Int             (Int32)
import           Data.Maybe           (fromMaybe)
import           Prelude
import           SplAST               (SplBinaryOperator (..),
                                       SplUnaryOperator (..))
import           SplIR
import           SplSSM

import qualified Data.Map             as Map

data Scope
    = Global
    | Local

type RegisterMap = Map.Map String (Offset, Scope)

data SSMState = SSMState {
        stackPtr  :: Int,
        globalMap :: RegisterMap,
        scopedMap :: RegisterMap
    }

type IRtoSSMState = WriterT [SSM] (State SSMState) ()

compileToSSM :: SplIR -> [SSM]
compileToSSM ir = evalState (execWriterT (programToSSM ir)) emptySSMState

emptySSMState :: SSMState
emptySSMState = SSMState {
    stackPtr = 0, globalMap = Map.empty, scopedMap = Map.empty}

out :: SSM -> IRtoSSMState
out x = tell [x]

pushOnStack :: String -> Size -> SSMState -> SSMState
pushOnStack r s st = st {
        scopedMap = Map.insert r (sp + 1, Local) $ scopedMap st,
        stackPtr = sp + s
    }
    where sp = stackPtr st

pushVariable :: String -> IRtoSSMState
pushVariable name = do
    modify $ pushOnStack name 1
    sp <- gets stackPtr
    out $ Comment $ "Stored " ++ name ++ " on stack at offset " ++ show sp

increaseStackPointer :: IRtoSSMState
increaseStackPointer = do
    sp' <- gets stackPtr
    let sp = sp' + 1
    out $ Comment $ "Incrementing SP to " ++ show sp
    modify $ \st -> st { stackPtr = sp }

decreaseStackPointer :: IRtoSSMState
decreaseStackPointer = do
    sp' <- gets stackPtr
    let sp = sp' - 1
    out $ Comment $ "Decrementing SP to " ++ show sp
    modify $ \st -> st { stackPtr = sp }

loadFromStack :: String -> IRtoSSMState
loadFromStack name = do
    (offset, loc) <- gets $ getStackVariable name
    case loc of
        Local -> do
            out $ Comment $ "Loading " ++ name ++ " from offset " ++ show offset
            out $ LDL offset
        Global -> do
            out $ Comment "FIXME: globals not yet implemented"
            out HALT
    increaseStackPointer

getStackVariable :: String -> SSMState -> (Offset, Scope)
getStackVariable name st =
    fromMaybe (error $ "You tried to find " ++ name ++ " on the stack, which I don't know")
              (Map.lookup name $ scopedMap st)

programToSSM :: SplIR -> IRtoSSMState
programToSSM ir = do
    let decls = filter (not . isFunction) ir
    let functions = filter isFunction ir

    -- cp SP to R5
    out $ LDRR R5 SP

    -- first generate code for declarations
    mapM_ toSSM decls

    -- make the generated locals the globals that they are
    vars <- gets scopedMap
    modify $ \st -> st {globalMap = Map.map (\(off, Local) -> (off, Global)) vars}

    -- generate branch to main
    when (any (isFunctionNamed "main") ir)
        (out $ BSR "main")

    -- We need to halt after return from main (or if there is no main).
    out HALT

    -- generate functions
    mapM_ toSSM functions

    -- would need to check if they're used
    tell isEmptySSM
    tell printSSM

    out HALT

    where
        isFunctionNamed needle (SplFunction name _ _) = name == needle
        isFunctionNamed _ _                           = False


-- gets something from somewhere on the stack (perhaps via a heap pointer) to the top
load :: SplPseudoRegister -> IRtoSSMState
load (Reg name)     = loadFromStack name
load (TupleFst reg) = loadFromHeap reg tupleFstHeapOffset
load (TupleSnd reg) = loadFromHeap reg tupleSndHeapOffset
load (ListHd reg)   = loadFromHeap reg tupleFstHeapOffset
load (ListTl reg)   = loadFromHeap reg tupleSndHeapOffset
load EmptyList      = do
    out $ Comment "pushing empty list (0-pointer)"
    out $ LDC 0
    increaseStackPointer

loadFromHeap :: SplPseudoRegister -> Size -> IRtoSSMState
loadFromHeap reg offset = do
    -- load address represented by reg
    out $ Comment $ "loading heap pointer " ++ show reg
    load reg
    -- use address at the top of the stack to load from heap at offset (reverse order)
    out $ LDA offset


tupleFstHeapOffset :: Int
tupleFstHeapOffset = -1

tupleSndHeapOffset :: Int
tupleSndHeapOffset = 0

listHdHeapOffset :: Int
listHdHeapOffset = -1

listTlHeapOffset :: Int
listTlHeapOffset = 0

store :: SplPseudoRegister -> IRtoSSMState
store (Reg name) = do
    -- look up name in stack administration
    lookupResult <- gets $ \st -> Map.lookup name $ scopedMap st
    case lookupResult of
        -- if in map: store top of stack to old location
        Just (offset, Local) -> out $ STL offset
        Just (offset, Global) -> do
            out $ LDR R5
            out $ STA offset
        -- if not in map, register current location in map
        Nothing -> pushVariable name
store (TupleFst reg) = storeToHeap reg tupleFstHeapOffset
store (TupleSnd reg) = storeToHeap reg tupleSndHeapOffset
store (ListHd reg) = storeToHeap reg listHdHeapOffset
store (ListTl reg) = storeToHeap reg listTlHeapOffset
store EmptyList    = error "Can't store to empty list"


storeToHeap :: SplPseudoRegister -> Size -> IRtoSSMState
storeToHeap reg offset = do
    -- load the address represented by reg
    load reg
    -- store the value, it's right before the address
    out $ STA offset


adjustStack :: Int -> IRtoSSMState
adjustStack 0 = tell []
adjustStack i = out $ AJS i

toSSM :: SplInstruction -> IRtoSSMState
toSSM (SplBinaryOperation op rd r1 r2) = do
    load r1
    load r2
    case op of
        SplOperatorAdd          -> out ADD
        SplOperatorSubtract     -> out SUB
        SplOperatorMultiply     -> out MUL
        SplOperatorDivide       -> out DIV
        SplOperatorModulus      -> out MOD
        SplOperatorLess         -> out SplSSM.LT
        SplOperatorLessEqual    -> out LE
        SplOperatorEqual        -> out SplSSM.EQ
        SplOperatorGreaterEqual -> out GE
        SplOperatorGreater      -> out SplSSM.GT
        SplOperatorNotEqual     -> out NE
        SplOperatorAnd          -> out AND
        SplOperatorOr           -> out OR
        SplOperatorCons         -> out $ STMH 2
    decreaseStackPointer -- eat two vars
    decreaseStackPointer
    store rd
toSSM (SplUnaryOperation op to from) = do
    load from
    case op of
        SplOperatorInvert -> out NOT
        SplOperatorNegate -> out NEG
    decreaseStackPointer
    store to
toSSM (SplRet register) = do
    -- fetch result register
    -- push return value in right place
    -- return?
    case register of
        Nothing      -> out $ Comment "return void"
        Just x -> do
            load x
            out $ STR RR
            decreaseStackPointer
    -- fix stack pointer
    spOffset <- gets stackPtr
    adjustStack $ -spOffset
    out $ STR MP
    out RET
toSSM (SplMov to from) = do
    load from
    decreaseStackPointer
    store to
toSSM (SplMovImm to (SplImmInt i)) = do
    when (i < toInteger (minBound :: Int32) || i > toInteger (maxBound :: Int32))
        $ error "SSM only supports signed 32-bit integers"
    -- set variable
    out $ Comment $ show to ++ " = " ++ show i
    out $ LDC (fromInteger i)
    store to
toSSM (SplMovImm to (SplImmBool i)) = do
    -- set variable
    out $ LDC $ if i then 1 else 0
    store to

toSSM (SplMovImm to (SplImmChar c)) = do
    -- set variable
    out $ LDC (ord c)
    store to

toSSM (SplTupleConstr dest left right) = do
    -- load left and right to top of stack
    load left
    load right
    -- store to heap
    out $ STMH 2
    decreaseStackPointer -- pops two
    decreaseStackPointer
    -- store address to dest
    store dest

toSSM (SplWhile label (condIR, condReg) loopIR) = do
    let endLabel = label ++ "end"
    preState <- get
    out $ Label label

    -- check condition
    out $ Comment "condition code"
    mapM_ toSSM condIR

    -- get rid of cond-local variables (due to unpacking of expressions)
    condStackPointer <- gets stackPtr
    adjustStack $ stackPtr preState - condStackPointer
    -- load branch condition
    load condReg
    -- reset state to before condition checking
    put preState
    -- jump to end if not met
    out $ BRF endLabel

    -- generate loop body
    out $ Comment $ "loop body for " ++ label
    mapM_ toSSM loopIR

    -- get rid of block-local variables (due to unpacking of expressions)
    endStackPointer <- gets stackPtr
    put preState
    adjustStack $ stackPtr preState - endStackPointer

    -- jump back to the start
    out $ Comment "Jump back to start of while loop"
    out $ BRA label
    out $ Label endLabel

toSSM (SplIf label condReg thenIR elseIR) = do
    out $ Label label
    preState <- get
    let elseLabel = label ++ "else"
    let endifLabel = label ++ "end"

    load condReg
    out $ BRF (if null elseIR then endifLabel else elseLabel)
    decreaseStackPointer

    mapM_ toSSM thenIR

    -- get rid of then-local variables (due to unpacking of expressions)
    thenStackPointer <- gets stackPtr
    adjustStack $ stackPtr preState - thenStackPointer
    put preState

    unless (null elseIR)
        (do
            out $ BRA endifLabel
            out $ Label elseLabel
        )

    mapM_ toSSM elseIR

    -- get rid of else-local variables (due to unpacking of expressions)
    elseStackPointer <- gets stackPtr
    adjustStack $ stackPtr preState - elseStackPointer
    put preState

    out $ Label endifLabel

toSSM (SplCall name result arguments) = do
    -- pop all arguments
    mapM_ load arguments
    -- branch to function
    out $ BSR name
    -- get rid of arguments
    out $ Comment "Drop arguments from stack"
    adjustStack $ -(length arguments)
    -- decrease stack pointer by the arguments we're removing
    modify $ \st -> st { stackPtr = stackPtr st - length arguments }

    -- load result register to finish up
    case result of
        Just r -> do
            out $ LDR RR
            store r
        Nothing -> out $ Comment "No result is used"

toSSM (SplFunction label args instrs) = do
    {- call semantics:
        - arguments on stack
        - followed by return address
        - MP = start locals
        - SP = end locals
        - result stored in RR
        - return:
            - leave stack frame
            - pop MP
            - RET
        - callee adjusts stack for args and pushes RR
    -}
    -- set scope with the arguments
    -- clear out room for return value
    out $ Label label

    currentState <- get

    -- store MP
    out $ LDR MP

    -- set MP = SP
    out $ LDRR MP SP

    -- negative offset from MP
    let argumentOffset = -1  - length args
    let newScope = insertArguments args argumentOffset $ globalMap currentState
    let scopedState = currentState {
        scopedMap = newScope,
        stackPtr = 0
    }

    put scopedState

    -- generate instructions nested in this function
    mapM_ toSSM instrs

    -- generate a return, although that may be rubbish
    out $ Comment "End of function return, may be BS"
    toSSM (SplRet Nothing)

    -- fix state
    put currentState
    where
        insertArguments :: [SplPseudoRegister] -> Offset -> RegisterMap -> RegisterMap
        insertArguments [] _ st = st
        insertArguments (Reg x:xs) n st =
            Map.insert x (n, Local) (insertArguments xs (n+1) st)
        -- functions cannot be defined with accessors like "f(a.hd){}"
        insertArguments _ _ _  = error "Compiler error: Wrong argument type"


isEmptySSM :: [SSM]
isEmptySSM = [
        Label "isEmpty",
        -- gets a list ptr as argument at the top of the stackPtr before the return address
        LDS $ -1,
        LDC 0,
        SplSSM.EQ,
        STR RR,
        RET
    ]

printSSM :: [SSM]
printSSM = [
        Label "print",
        -- gets a character as an argument at the top of the stackPtr before the return address
        LDS $ -1,
        TRAP PopPrintChar,
        RET
    ]
