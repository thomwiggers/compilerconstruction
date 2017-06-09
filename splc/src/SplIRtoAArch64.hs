module SplIRtoAArch64 (module SplIRtoAArch64) where

import SplAST (SplBinaryOperator(..), SplUnaryOperator(..))
import SplIR
import SplAArch64
import SplAArch64Allocator

import qualified SplAArch64StdLib as StdLib

import Control.Monad.State
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

data Scope = Global | Local

data AArch64State = AArch64State {
        globalMap :: Map.Map String Scope,
        scopedMap :: Map.Map String Scope
    }

emptyAArch64State :: AArch64State
emptyAArch64State = AArch64State {globalMap = Map.empty,
                                  scopedMap = Map.empty}

type IRtoAArch64State = State AArch64State [AArch64Instruction]

compileToAArch64 :: SplIR -> String
compileToAArch64 ir =
    StdLib.programPreamble ++ "\n\n" ++
    -- function to set locale and call initGlobals
    StdLib.initFunction ++ "\n\n" ++
    -- initialise the globals
    printList (allocateRegisters globalCode) ++ "\n\n" ++
    -- program source code
    printList (allocateRegisters programCode) ++ "\n\n" ++
    -- stdlib
    StdLib.print ++ "\n\n" ++
    StdLib.printInt ++ "\n\n" ++
    StdLib.isEmpty ++ "\n\n" ++
    StdLib.constants ++ "\n\n" ++
    generateGlobalSegments globals
    where
        (globalCode, programCode, globals) = evalState (programToAArch64 ir) emptyAArch64State
        generateGlobalSegments [] = ""
        generateGlobalSegments (name:names) =
                    prefixGlob name ++ ":\n" ++
                    "\t.size " ++ prefixGlob name ++ ", " ++ show wordLength ++ "\n\n" ++
                    generateGlobalSegments names

prefixGlob :: String -> String
prefixGlob = (++) ".GLOB_"

programToAArch64 :: SplIR -> State AArch64State ([AArch64Instruction], [AArch64Instruction], [String])
programToAArch64 ir = do
    let decls = filter (not . isFunction) ir
    let functions = filter isFunction ir

    -- collect names of global variables
    let globalNames = assignmentNames decls
    let globMap = Map.fromList [(x, Global) | x <- globalNames]
    modify $ \st -> st{globalMap = globMap}

    globalCode <- toAArch64 (SplFunction "initGlobals" [] decls)
    functionCode <- concat <$> mapM toAArch64 functions

    return (globalCode, functionCode, globalNames)

load :: SplPseudoRegister -> State AArch64State ([AArch64Instruction], Register)
load (Reg name) = do
    maybeKind <- gets $ \st -> Map.lookup name (scopedMap st)
    let kind = fromMaybe (error "compiler error: trying to load an uninitialised register")
                         maybeKind
    case kind of
        Local -> return ([], PR name)
        Global -> return (loadFromGlobal name, PR name)

store :: SplPseudoRegister -> State AArch64State ([AArch64Instruction], Register)
store (Reg name) = do
    maybeKind <- gets $ \st -> Map.lookup name (scopedMap st)
    kind <- case maybeKind of
        Just k -> return k
        Nothing -> do
            modify $ \st -> st{scopedMap = Map.insert name Local (scopedMap st)}
            return Local
    case kind of
        Local -> return ([], PR name)
        Global -> return (storeToGlobal name, PR name)

storeToGlobal :: String -> [AArch64Instruction]
storeToGlobal name = [ADRP (PR adrName) (prefixGlob name),
                      AddNamedOffset (PR adrName) (PR adrName) (prefixGlob name),
                      STR (PR name) (Address (PR adrName) 0)]
        where adrName = "_adr_" ++ name

loadFromGlobal :: String -> [AArch64Instruction]
loadFromGlobal name = [ADRP (PR adrName) (prefixGlob name),
                       AddNamedOffset (PR adrName) (PR adrName) (prefixGlob name),
                       LDR (PR name) (Address (PR adrName) 0)]
        where adrName = "_adr_" ++ name

toAArch64 :: SplInstruction -> IRtoAArch64State
-- binary operations
toAArch64 (SplBinaryOperation operator rd rn rm) = do
    (loadnCode, nReg) <- load rn
    (loadmCode, mReg) <- load rm
    (storeCode, dReg) <- store rd
    let opCode = case operator of
            SplOperatorAdd -> [ADD dReg nReg mReg]
            SplOperatorSubtract -> [SUB dReg nReg mReg]
            SplOperatorMultiply -> [MUL dReg nReg mReg]
            SplOperatorDivide -> [SDIV dReg nReg mReg]
            SplOperatorEqual -> [CMP nReg mReg, CSET dReg Equal]
            SplOperatorNotEqual -> [CMP nReg mReg, CSET dReg NotEqual]
            SplOperatorLess -> [CMP nReg mReg, CSET dReg Less]
            SplOperatorLessEqual -> [CMP nReg mReg, CSET dReg LessEqual]
            SplOperatorGreater -> [CMP nReg mReg, CSET dReg Greater]
            SplOperatorGreaterEqual -> [CMP nReg mReg, CSET dReg GreaterEqual]
            SplOperatorAnd -> [AND dReg nReg mReg]
            SplOperatorOr -> [OR dReg nReg mReg]
            SplOperatorModulus -> [SDIV dReg nReg mReg, MSUB dReg dReg mReg nReg] -- Rd = Rn / Rm; Rd = -(Rd * Rm) + Rn
            SplOperatorCons -> error "lists not yet implemented"
    return $ loadnCode ++ loadmCode ++ opCode ++ storeCode

-- unary operations:
toAArch64 (SplUnaryOperation operator rd rm) = do
    (loadnCode, mReg) <- load rm
    (storeCode, dReg) <- store rd
    let opCode = case operator of
            SplOperatorInvert -> EOR dReg mReg (Imm 1)
            SplOperatorNegate -> NEG dReg mReg
    return $ loadnCode ++ [opCode] ++ storeCode

-- move
toAArch64 (SplMov rd rm) = do
    (loadnCode, mReg) <- load rm
    (storeCode, dReg) <- store rd
    return $ loadnCode ++ [MOV dReg mReg] ++ storeCode
toAArch64 (SplMovImm rd i) = do
    (storeCode, dReg) <- store rd
    let opCode = case i of
            SplImmInt n -> [MOV dReg (Imm $ fromInteger n)]
            SplImmBool b -> [MOV dReg (Imm $ if b then 1 else 0)]
            SplImmChar c ->
                let charCode = ord c in
                if charCode < 65536 -- < 2^16
                    then [MOV dReg (Imm charCode)]
                    -- encode higher unicode characters by adding up two 16-bit imms
                    else [MOV dReg (Imm (charCode `mod` 65536)),
                          MOVK dReg (Imm (charCode - (charCode `mod` 65536)))]
    return $ opCode ++ storeCode

-- return
toAArch64 (SplRet Nothing) = return [RET]
toAArch64 (SplRet (Just r)) = do
    (loadCode, reg) <- load r
    return $ loadCode ++ [MOV (X 0) reg, RET]

-- functions
toAArch64 (SplFunction label [] ir) = do
    -- FIXME no arguments
    -- Make sure to first copy x0, .. x7 to temps to not clash
    -- with register allocation of the return register etc?

    -- set scoped map
    modify $ \st -> st{scopedMap = globalMap st}

    functionCode <- concat <$> mapM toAArch64 ir
    return [BasicBlock $ Label (functionName label) : functionCode ++ [RET]]

-- if
toAArch64 (SplIf label cond thenIR elseIR) = do
    let elseLabel = label ++ "else"
    let endifLabel = label ++ "end"
    (condCode, condReg) <- load cond
    let ifStart = [Comment $ "if " ++ label] ++
                  condCode ++
                  [CMP condReg (Imm 0),
                   BranchConditional Equal elseLabel]

    thenCode <- concat <$> mapM toAArch64 thenIR

    elseCode <- if not (null elseIR)
        then (do
            elseCode' <- concat <$> mapM toAArch64 elseIR
            return $ [BranchAlways endifLabel,
                      Label elseLabel] ++
                      elseCode' ++
                      [Label endifLabel]
        )
        else return [Label elseLabel]

    return [BasicBlock ifStart, BasicBlock thenCode, BasicBlock elseCode]

-- while
toAArch64 (SplWhile label (condIR, cond) bodyIR) = do
    let endLabel = label ++ "end"
    condCode <- concat <$> mapM toAArch64 condIR
    (condRegCode, condReg) <- load cond
    let condBlock = BasicBlock $ [Label label] ++
                     condCode ++ condRegCode ++
                     [CMP condReg (Imm 0),
                     BranchConditional Equal endLabel]

    bodyCode <- concat <$> mapM toAArch64 bodyIR
    let bodyBlock = BasicBlock $ bodyCode ++
                                [BranchAlways label,
                                 Label endLabel]

    return [condBlock, bodyBlock]
