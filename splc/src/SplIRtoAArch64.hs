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

data Scope = Global | Local | StackArgument Offset

data AArch64State = AArch64State {
        globalMap :: Map.Map Register Scope,
        scopedMap :: Map.Map Register Scope,
        frameSize :: Int
    }

emptyAArch64State :: AArch64State
emptyAArch64State = AArch64State {globalMap = Map.empty,
                                  scopedMap = Map.empty,
                                  frameSize = 0
                                 }

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
                    "\t.space " ++ show wordLength ++ ", 0" ++ "\n\n" ++
                    generateGlobalSegments names

prefixGlob :: String -> String
prefixGlob = (++) ".GLOB_"


tupleFstHeapOffset :: Int
tupleFstHeapOffset = 0 * wordLength

tupleSndHeapOffset :: Int
tupleSndHeapOffset = 1 * wordLength

listHdHeapOffset :: Int
listHdHeapOffset = 0 * wordLength

listTlHeapOffset :: Int
listTlHeapOffset = 1 * wordLength

heapPtr :: Register
heapPtr = X 28

programToAArch64 :: SplIR -> State AArch64State ([AArch64Instruction], [AArch64Instruction], [String])
programToAArch64 ir = do
    let decls = filter (not . isFunction) ir
    let functions = filter isFunction ir

    -- collect names of global variables
    let globalNames = assignmentNames decls
    let globMap = Map.fromList [(PR x, Global) | x <- globalNames]
    modify $ \st -> st{globalMap = globMap}

    globalCode <- toAArch64 (SplFunction "_initGlobals" [] decls)
    functionCode <- concat <$> mapM toAArch64 functions

    return (globalCode, functionCode, globalNames)

load :: SplPseudoRegister -> State AArch64State ([AArch64Instruction], Register)
load (Reg name) = do
    maybeKind <- gets $ \st -> Map.lookup (PR name) (scopedMap st)
    let kind = fromMaybe (error "compiler error: trying to load an uninitialised register")
                         maybeKind
    case kind of
        Local -> return ([], PR name)
        Global -> return (loadFromGlobal name, PR name)
        StackArgument offset -> do
            -- change loaded stackarg to local var to prevent multiple loads
            modify $ \st -> st{scopedMap = Map.insert (PR name) Local (scopedMap st)}
            return ([LDR (PR name) (Address SP offset)], PR name)
load r@(TupleFst reg) = loadFromHeap reg (getRegisterName r) tupleFstHeapOffset
load r@(TupleSnd reg) = loadFromHeap reg (getRegisterName r) tupleSndHeapOffset
load r@(ListHd reg) = loadFromHeap reg (getRegisterName r) listHdHeapOffset
load r@(ListTl reg) = loadFromHeap reg (getRegisterName r) listTlHeapOffset
load EmptyList = return ([MOV (PR "emptyList") (Imm 0)], PR "emptyList")

getRegisterName :: SplPseudoRegister -> String
getRegisterName (Reg name) = name
getRegisterName (TupleFst r) = "_fst_" ++ getRegisterName r
getRegisterName (TupleSnd r) = "_snd_" ++ getRegisterName r
getRegisterName (ListHd r) = "_hd_" ++ getRegisterName r
getRegisterName (ListTl r) = "_tl_" ++ getRegisterName r
getRegisterName EmptyList = "emptyList"

loadFromHeap :: SplPseudoRegister -> String -> Offset -> State AArch64State ([AArch64Instruction], Register)
loadFromHeap reg targetRegName offset = do
    -- load address represented by reg
    let comment = Comment $ "loading heap pointer " ++ show reg
    (instr, addrReg) <- load reg
    let newReg = PR targetRegName
    let loadInstr = instr ++ [LDR newReg (Address addrReg offset)]
    return (comment : loadInstr, newReg)


store :: SplPseudoRegister -> State AArch64State ([AArch64Instruction], Register)
store (Reg name) = do
    maybeKind <- gets $ \st -> Map.lookup (PR name) (scopedMap st)
    kind <- case maybeKind of
        Just k -> return k
        Nothing -> do
            modify $ \st -> st{scopedMap = Map.insert (PR name) Local (scopedMap st)}
            return Local
    case kind of
        Local -> return ([], PR name)
        Global -> return (storeToGlobal name, PR name)
        StackArgument _ -> do
            -- assigning into stack arguments turns them into a local var
            -- only occurs when it's not already been loaded
            modify $ \st -> st{scopedMap = Map.insert (PR name) Local (scopedMap st)}
            return ([], PR name)
store r@(TupleFst reg) = storeToHeap reg (getRegisterName r) tupleFstHeapOffset
store r@(TupleSnd reg) = storeToHeap reg (getRegisterName r) tupleSndHeapOffset
store r@(ListHd reg) = storeToHeap reg (getRegisterName r) listHdHeapOffset
store r@(ListTl reg) = storeToHeap reg (getRegisterName r) listTlHeapOffset
store EmptyList    = error "Can't store to empty list"

storeToHeap :: SplPseudoRegister -> String -> Offset -> State AArch64State ([AArch64Instruction], Register)
storeToHeap reg targetRegName offset = do
    -- load address represented by reg
    let comment = Comment $ "loading heap pointer " ++ show reg
    (instr, addrReg) <- load reg
    let newReg = PR targetRegName
    let loadInstr = instr ++ [STR newReg (Address addrReg offset)]
    return (comment : loadInstr, newReg)


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

spill :: Register -> IRtoAArch64State
spill r = do
    offset <- gets frameSize
    modify $ \st -> st{frameSize = offset + wordLength,
                       scopedMap = Map.insert r (StackArgument offset) (scopedMap st)
                    }
    return [STR r (Address SP (-offset))]

unspill :: Register -> IRtoAArch64State
unspill r = do
    maybeRegister <- gets $ Map.lookup r <$> scopedMap
    case maybeRegister of
        Just (StackArgument offset) ->
            return [LDR r (Address SP offset)]
        _ -> error "Not spilled"


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
            SplOperatorCons -> [STP nReg mReg (Address heapPtr 0),
                                MOV dReg heapPtr,
                                ADD heapPtr heapPtr (Imm $ 2 * wordLength)]
    return $ loadnCode ++ loadmCode ++ opCode ++ storeCode

toAArch64 (SplTupleConstr d l r) = do
    (loadlCode, lReg) <- load l
    (loadrCode, rReg) <- load r
    (storeCode, dReg) <- store d

    let tupleCode = [STP lReg rReg (Address heapPtr 0),
                     MOV dReg heapPtr,
                     ADD heapPtr heapPtr (Imm $ 2 * wordLength)]
    return $ loadlCode ++ loadrCode ++ tupleCode ++ storeCode
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
toAArch64 (SplRet Nothing) = do
    lrRestoreCode <- unspill LR
    return $ lrRestoreCode ++ [RET]
toAArch64 (SplRet (Just r)) = do
    (loadCode, reg) <- load r
    lrRestoreCode <- unspill LR
    return $ loadCode ++ lrRestoreCode ++ [MOV (X 0) reg, RET]

-- function call
toAArch64 (SplCall label maybeResult args) = do
    let (regArgs, stackArgs) = splitAt 8 args

    -- prepare arguments
    (argInstrs, argRegs) <- unzip <$> mapM load regArgs

    -- spill x0-x7,
    spillInstrs <- concat <$> mapM spill [X i | i <- [0..7]]

    -- put arguments in right registers
    let regArgMovs = zipWith MOV [X i | i <- [0..]] argRegs

    -- modify stack pointer with frame Size
    fs <- gets frameSize
    -- sp must be 16-bit aligned
    let alignedFs = if fs `mod` 16 == 0 then fs else 16 * ((fs `div` 16) + 1)
    let spAdjust = [SUB SP SP (Imm alignedFs)]

    -- prepare and push arguments that are to be put on stack
    stackArgInstr <- concat <$> mapM loadPush stackArgs

    -- call
    let callInstr = [BranchLink label]

    -- restore stack pointer
    let spRestore = [ADD SP SP (Imm alignedFs)]

    -- get result
    resultInstrs <- case maybeResult of
            Just rReg -> do
                (storeInstrs, resultReg) <- store rReg
                return (MOV resultReg (X 0) : storeInstrs)
            Nothing -> return []

    -- restore spilled registers
    let (unspillX0 : unspillInstrs) = map (\(STR r a) -> LDR r a) spillInstrs
    -- restore sp to normality
    modify $ \st -> st{frameSize = fs - (length unspillInstrs + 1) * wordLength}

    return $ concat argInstrs ++
            spillInstrs ++
            regArgMovs ++
            spAdjust ++
            stackArgInstr ++
            callInstr ++
            spRestore ++
            unspillInstrs ++
            resultInstrs ++
            -- unspill X0 separately because we don't want to overwrite the result
            -- register before we can move it out
            [unspillX0]
    where
        loadPush arg = do
            (instr, _) <- load arg
            (storeInstr, _) <- store arg
            return (instr ++ storeInstr)

-- functions
toAArch64 (SplFunction label args ir) = do
    -- set scoped map
    let (regArgs, stackArgs) = splitAt 8 args
    modify $ \st -> st{
        scopedMap =
            globalMap st
            `Map.union`
            Map.fromList [(PR a, Local) | (Reg a) <- regArgs]
            `Map.union`
            Map.fromList [(PR a, StackArgument $ i * wordLength) | (i, Reg a) <- zip [0..] stackArgs],
        frameSize = length stackArgs * wordLength
        }

    -- generate movs from hw regs to pseudo regs to aid allocator
    let allocMovs = zipWith (\x (Reg r) -> MOV (PR r) x) [X i | i <- [0..]] regArgs

    -- spill lr
    spillLRCode <- spill LR

    functionBody <- concat <$> mapM toAArch64 ir

    let unspillLRCode = map (\(STR r a) -> LDR r a) spillLRCode

    return [BasicBlock $ Label (functionName label) : spillLRCode ++ allocMovs ++ functionBody ++ unspillLRCode ++ [RET]]

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
