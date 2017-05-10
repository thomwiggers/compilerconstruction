{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-unused-matches #-}
module SplIRtoSSM where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Int             (Int32)
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import           Prelude
import           SplAST               (SplBinaryOperator (..),
                                       SplUnaryOperator (..))
import           SplIR

import           SplSSM

data Scope
    = Global
    | Local

data SSMState = SSMState {
        stackPtr  :: Int,
        globalMap :: Map.Map String (Offset, Scope),
        scopedMap :: Map.Map String (Offset, Scope)
    }

type IRtoSSMState = WriterT [SSM] (State SSMState) ()

sizeOf :: SplPseudoRegister -> Int
sizeOf (Reg _)     = 1
sizeOf (Tuple l r) = sizeOf l + sizeOf r
sizeOf (List _ _)  = 1       -- heap ptr
sizeOf EmptyList   = 1      -- null ptr

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
        SplOperatorEqual        -> out CMP
        SplOperatorGreaterEqual -> out GE
        SplOperatorGreater      -> out SplSSM.GT
        SplOperatorNotEqual     -> out CMP >> out NOT
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
toSSM (SplRet register) =
    -- fetch result register
    -- push return value in right place
    -- return?
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

toSSM (SplFunction label _ _) = do
    -- determine call semantics (arguments on stack in order?)
    -- set scope with the arguments
    -- clear out room for return value
    out $ Label label
    out HALT


isEmptySSM :: [SSM]
isEmptySSM = [
        Label "isEmpty",
        -- gets a list ptr as argument at the top of the stackPtr
        LDC 0,
        CMP,
        -- FIXME write result
        RET
    ]
