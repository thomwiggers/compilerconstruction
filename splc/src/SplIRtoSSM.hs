{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-unused-matches #-}
module SplIRtoSSM where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Int             (Int32)
import           Data.Char            (ord)
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
    sp <- gets stackPtr
    modify $ pushOnStack name 1
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
    fromMaybe (error "You tried to find something on the stack I don't know")
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


load :: SplPseudoRegister -> IRtoSSMState
load (Reg name) = loadFromStack name
load (TupleFst reg) = loadFromHeap reg $ -1
load (TupleSnd reg) = loadFromHeap reg 0
load (ListHd reg) = loadFromHeap reg $ -1
load (ListTl reg) = loadFromHeap reg 0


loadFromHeap :: SplPseudoRegister -> Size -> IRtoSSMState
loadFromHeap reg offset = do
    -- load address represented by reg
    load reg
    -- use address at the top of the stack to load from heap at offset (reverse order)
    out $ LDA offset


store :: SplPseudoRegister -> IRtoSSMState
store (Reg name) = do
    -- look up name in stack administration
    lookupResult <- gets $ \st -> Map.lookup name $ scopedMap st
    case lookupResult of
        -- if in map: store top of stack to old location
        Just (offset, Local) -> do
            out $ STL offset
            decreaseStackPointer
        Just (offset, Global) -> error "do iets met globals"
        -- if not in map, register current location in map
        Nothing -> pushVariable name
store (TupleFst reg) = storeToHeap reg $ -1
store (TupleSnd reg) = storeToHeap reg 0
store (ListHd reg) = storeToHeap reg $ -1
store (ListTl reg) = storeToHeap reg 0


storeToHeap :: SplPseudoRegister -> Size -> IRtoSSMState
storeToHeap reg offset = do
    -- load the address represented by reg
    load reg
    -- store the value, it's right before the address
    out $ STA offset


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
        SplOperatorCons         -> error "don't know how to handle lists yet"
    decreaseStackPointer -- eat two vars
    decreaseStackPointer
    store rd
toSSM (SplUnaryOperation op (Reg to) (Reg from)) = do
    loadFromStack from
    case op of
        SplOperatorInvert -> out NOT
        SplOperatorNegate -> out NEG
    decreaseStackPointer
    pushVariable to
toSSM (SplJump label) = out $ BRA label
toSSM (SplJumpIf (Reg cond) label) = do
    out $ Comment $ "jump if " ++ cond
    loadFromStack cond
    out $ BRT label
    -- brt eats the top element on the stack
    decreaseStackPointer
toSSM (SplJumpIfNot (Reg cond) label) = do
    out $ Comment $ "jump if not " ++ cond
    loadFromStack cond
    out $ BRF label
    -- brt eats the top element from the stack
    decreaseStackPointer
toSSM (SplJumpTarget label) = out $ Label label
toSSM (SplRet register) = do
    -- fetch result register
    -- push return value in right place
    -- return?
    case register of
        Nothing      -> out $ Comment "return void"
        Just (Reg x) -> do
            loadFromStack x
            out $ STR RR
            decreaseStackPointer
    -- fix stack pointer
    spOffset <- gets stackPtr
    out $ AJS $ -spOffset
    out $ STR MP
    out RET
toSSM (SplMov (Reg to) (Reg from)) = do
    loadFromStack from
    pushVariable to
toSSM (SplMovImm (Reg to) (SplImmInt i)) = do
    when (i < toInteger (minBound :: Int32) || i > toInteger (maxBound :: Int32))
        $ error "SSM only supports signed 32-bit integers"
    -- set variable
    out $ LDC (fromInteger i)
    pushVariable to
toSSM (SplMovImm (Reg to) (SplImmBool i)) = do
    -- set variable
    out $ LDC $ if i then 0 else 1
    pushVariable to

toSSM (SplMovImm (Reg to) (SplImmChar c)) = do
    -- set variable
    out $ LDC (ord c)
    pushVariable to

toSSM (SplCall name arguments) = do
    -- pop all arguments
    mapM_ load arguments
    -- branch to function
    out $ BSR name
    -- get rid of arguments
    out $ AJS (-(length arguments))
    -- load result register to finish up
    out $ LDR RR
    increaseStackPointer

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
    let argumentOffset = -2  - length args
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
