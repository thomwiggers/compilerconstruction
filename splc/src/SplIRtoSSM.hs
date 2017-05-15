{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-unused-matches #-}
module SplIRtoSSM where

import           Control.Monad.State
import           Control.Monad.Writer
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
        scopedMap = Map.insert r (sp, Local) $ scopedMap st,
        stackPtr = sp + s
    }
    where sp = stackPtr st

increaseStackPointer :: IRtoSSMState
increaseStackPointer = modify $ \st -> st { stackPtr = stackPtr st + 1}

decreaseStackPointer :: IRtoSSMState
decreaseStackPointer = modify $ \st -> st { stackPtr = stackPtr st - 1}

loadFromStack :: String -> IRtoSSMState
loadFromStack name = do
    (offset, loc) <- gets $ getStackVariable name
    case loc of
        Local -> do
            out $ Comment $ "Loading " ++ name
            out $ LDL offset
            increaseStackPointer
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
    out $ BSR "main"
    -- We need to halt after return from main.
    out HALT

    -- generate functions
    mapM_ toSSM functions

    tell isEmptySSM

    out HALT

toSSM :: SplInstruction -> IRtoSSMState
toSSM (SplBinaryOperation op (Reg rd) (Reg r1) (Reg r2)) = do
    loadFromStack r1
    loadFromStack r2
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
    modify $ pushOnStack rd 1
toSSM (SplUnaryOperation op (Reg to) (Reg from)) = do
    loadFromStack from
    case op of
        SplOperatorInvert -> out NOT
        SplOperatorNegate -> out NEG
    decreaseStackPointer
    modify $ pushOnStack to 1
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
        Just (Reg x) -> out $ STR RR
    -- fix stack pointer
    spOffset <- gets stackPtr
    out $ STR MP
    out $ AJS $ -(spOffset  - 1)  -- STR MP also decrements by 1
    out RET
toSSM (SplMov (Reg to) (Reg from)) = do
    loadFromStack from
    modify $ pushOnStack to 1
toSSM (SplMovImm (Reg to) (SplImmInt i)) = do
    when (i < toInteger (minBound :: Int32) || i > toInteger (maxBound :: Int32))
        $ error "SSM only supports signed 32-bit integers"
    -- set variable
    out $ LDC (fromInteger i)
    modify $ pushOnStack to 1
toSSM (SplMovImm (Reg to) (SplImmBool i)) = do
    -- set variable
    out $ LDC $ if i then 0 else 1
    modify $ pushOnStack to 1

toSSM (SplCall name arguments) = do
    -- pop all arguments
    -- branch to function
    out $ BSR name
    -- get rid of arguments
    out $ AJS (-(length arguments))
    -- load result register to finish up
    out $ LDR RR

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
    increaseStackPointer

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

    -- fix state
    put currentState
    decreaseStackPointer
    out RET
    where
        insertArguments :: [SplPseudoRegister] -> Offset -> RegisterMap -> RegisterMap
        insertArguments [] _ st = st
        insertArguments (Reg x:xs) n st =
            Map.insert x (n, Local) (insertArguments xs (n+1) st)


isEmptySSM :: [SSM]
isEmptySSM = [
        Label "isEmpty",
        -- gets a list ptr as argument at the top of the stackPtr
        LDC 0,
        SplSSM.EQ,
        STR RR,
        RET
    ]
