module SplAArch64Allocator (allocateRegisters, annotateInstructions) where

import Control.Arrow (second)
import           Control.Monad.State

import           Data.List           (delete, (\\))
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import qualified Data.Set            as Set

import           SplAArch64

-- instructions with the registers that are live *after* that point
type AnnotatedInstructions = [(AArch64Instruction, Set.Set Register)]

allocateRegisters :: [AArch64Instruction] -> [AArch64Instruction]
allocateRegisters instrs =
    evalState (assignRegisters $ annotateInstructions instrs) emptyAllocatorState

annotateInstructions :: [AArch64Instruction] -> AnnotatedInstructions
annotateInstructions z =
     map (second (Set.filter notImmediate)) $
        reverse $ annotate (reverse z) Set.empty
    where
        annotate [] _  = []
        annotate (x:xs) prev_live = (x, new_live) : annotate xs new_live
            where
                live_without_output = prev_live Set.\\ output x
                new_live = live_without_output `Set.union` inputs x

        notImmediate (Imm _) = False
        notImmediate _       = True


inputs :: AArch64Instruction -> Set.Set Register
inputs (ADD _ a b)           = Set.fromList [a, b]
inputs (MUL _ a b)           = Set.fromList [a, b]
inputs (SDIV _ a b)          = Set.fromList [a, b]
inputs (SUB _ a b)           = Set.fromList [a, b]
inputs (NEG _ a)             = Set.singleton a
inputs (MSUB _ a b c)        = Set.fromList [a, b, c]
inputs (AND _ a b)           = Set.fromList [a, b]
inputs (OR _ a b)            = Set.fromList [a, b]
inputs (EOR _ a b)           = Set.fromList [a, b]
inputs (CMP a b)             = Set.fromList [a, b]
inputs CSET{}                = Set.empty
inputs (MOV _ a)             = Set.singleton a
inputs (MOVK _ a)            = Set.singleton a
inputs BranchConditional{}   = Set.empty
inputs BranchAlways{}        = Set.empty
inputs BranchLink{}          = Set.empty
inputs (LDR _ (Address r _)) = Set.singleton r
inputs (STR r (Address a _)) = Set.fromList [r, a]
inputs (STP r s (Address a _)) = Set.fromList [r, s, a]
inputs ADRP{}                = Set.empty
inputs (AddNamedOffset _ r _) = Set.singleton r
inputs RET                   = Set.empty
inputs NOP                   = Set.empty
inputs Comment{}             = Set.empty
inputs Label{}               = Set.empty
inputs (BasicBlock instrs)   = head . map snd $ annotateInstructions instrs

output :: AArch64Instruction -> Set.Set Register
output (ADD o _ _)         = Set.singleton o
output (MUL o _ _)         = Set.singleton o
output (SDIV o _ _)        = Set.singleton o
output (SUB o _ _)         = Set.singleton o
output (NEG o _)           = Set.singleton o
output (MSUB o _ _ _)      = Set.singleton o
output (AND o _ _)         = Set.singleton o
output (OR o _ _)          = Set.singleton o
output (EOR o _ _)         = Set.singleton o
output CMP{}               = Set.empty
output (CSET a _)          = Set.singleton a
output (MOV o _)           = Set.singleton o
output (MOVK o _)          = Set.singleton o
output BranchConditional{} = Set.empty
output BranchAlways{}      = Set.empty
output BranchLink{}        = Set.empty
output (LDR r _)           = Set.singleton r
output STR{}               = Set.empty
output STP{}               = Set.empty
output (ADRP r _)          = Set.singleton r
output (AddNamedOffset r _ _) = Set.singleton r
output RET                 = Set.empty
output NOP                 = Set.empty
output Comment{}           = Set.empty
output Label{}             = Set.empty
output (BasicBlock _)      = Set.empty -- for now we assume this can be empty


data AllocatorState = AllocatorState {
        freeRegisters :: [Register],
        usedRegisters :: [Register],
        prToRegMap    :: Map.Map String Register,
        parentRegisters :: [Register] -- registers to keep while in a block
    }

emptyAllocatorState :: AllocatorState
emptyAllocatorState = AllocatorState {
        freeRegisters = [X i | i <- [0..27]],
        usedRegisters = [],
        prToRegMap = Map.empty,
        parentRegisters = []
    }

popFreeRegister :: State AllocatorState Register
popFreeRegister = do
    regs <- gets freeRegisters
    when (null regs) $ error "no more free registers (spilling not implemented)"
    let newReg = head regs
    modify $ \st -> st{freeRegisters = tail regs,
                       usedRegisters = newReg : usedRegisters st}
    return newReg

releaseRegister :: Register -> State AllocatorState ()
releaseRegister reg@(X _) = do
    regs <- gets freeRegisters
    when (reg `elem` regs) $
        error $ "compiler error: register " ++ show reg ++ " has already been freed"
    modify $ \st -> st{freeRegisters = reg : regs,
                       usedRegisters = delete reg $ usedRegisters st}
releaseRegister _ = error "Compiler error: releaseRegister only works on real registers for now"


getRegisterFor :: Register -> State AllocatorState Register
getRegisterFor (PR n) = do
    newReg <- popFreeRegister
    modify $ \st -> st{prToRegMap = Map.insert n newReg (prToRegMap st)}
    return newReg
getRegisterFor _ = error "compiler error: can only get a register for pseudo-registers"

dropRegisterFor :: Register -> State AllocatorState ()
dropRegisterFor (PR n) = do
    reg <- inputRegister (PR n)
    releaseRegister reg
    modify $ \st -> st{prToRegMap = Map.delete n (prToRegMap st)}
dropRegisterFor r@X{} = releaseRegister r
dropRegisterFor _ = error "compiler error: can only release pseudo or real register"

dropRegisters :: [Register] -> State AllocatorState ()
dropRegisters [] = return ()
dropRegisters (x:xs) = do
    dropRegisterFor x
    dropRegisters xs

lookupPseudoRegister :: String -> State AllocatorState (Maybe Register)
lookupPseudoRegister name = gets $ Map.lookup name <$> prToRegMap

inputRegister :: Register -> State AllocatorState Register
inputRegister (PR n) =
        fromMaybe (error $ "Input " ++ n ++ " used before assignment")
    <$> lookupPseudoRegister n
inputRegister r = return r

outputRegister :: Register -> State AllocatorState Register
outputRegister Imm{} = error "compiler error: can't assign to immediate"
outputRegister (PR n) = do
    reg <- lookupPseudoRegister n
    case reg of
        Nothing  -> getRegisterFor (PR n)
        (Just r) -> return r
outputRegister r = return r

assignRegisters :: AnnotatedInstructions -> State AllocatorState [AArch64Instruction]
assignRegisters [] = return []
assignRegisters ((instruction, inputRegs) : xs) = do
    -- free all used registers that are no longer in inputs
    regMap <- gets prToRegMap
    let inputHWRegs = Set.toList $ Set.map (findRegisterInMap regMap) inputRegs
    currentlyUsed <- gets usedRegisters
    parentRegs <- gets parentRegisters
    let obsoleteRegs = (currentlyUsed \\ inputHWRegs) \\ parentRegs
    dropRegisters obsoleteRegs
    -- drop the PRs from the map, whose hw regs we've just deleted
    modify $ \st -> st{prToRegMap = Map.filter (`notElem` obsoleteRegs) regMap}

    -- substitute current instruction
    substitutedInstruction <- assignRegistersToInstruction instruction
    -- recursive call
    restInstructions <- assignRegisters xs

    return $ substitutedInstruction : restInstructions
    where
        findRegisterInMap :: Map.Map String Register -> Register -> Register
        findRegisterInMap m (PR n) =
            fromMaybe (error $ "compiler error: PR " ++ n ++ " used as input before assignment")
                $ Map.lookup n m
        findRegisterInMap _ r = r

assignRegistersToInstruction :: AArch64Instruction -> State AllocatorState AArch64Instruction
assignRegistersToInstruction (ADD rd ra rb) = ADD <$> outputRegister rd <*> inputRegister ra <*> inputRegister rb
assignRegistersToInstruction (MUL rd ra rb) = MUL <$> outputRegister rd <*> inputRegister ra <*> inputRegister rb
assignRegistersToInstruction (SDIV rd ra rb) = SDIV <$> outputRegister rd <*> inputRegister ra <*> inputRegister rb
assignRegistersToInstruction (SUB rd ra rb) = SUB <$> outputRegister rd <*> inputRegister ra <*> inputRegister rb
assignRegistersToInstruction (NEG rd ra) = NEG <$> outputRegister rd <*> inputRegister ra
assignRegistersToInstruction (MSUB rd rn rm ra) =
    MSUB <$> outputRegister rd <*> inputRegister rn <*> inputRegister rm <*> inputRegister ra
assignRegistersToInstruction (AND rd ra rb) = AND <$> outputRegister rd <*> inputRegister ra <*> inputRegister rb
assignRegistersToInstruction (OR rd ra rb) = OR <$> outputRegister rd <*> inputRegister ra <*> inputRegister rb
assignRegistersToInstruction (EOR rd ra rb) = EOR <$> outputRegister rd <*> inputRegister ra <*> inputRegister rb
assignRegistersToInstruction (CMP ra rb) = CMP <$> inputRegister ra <*> inputRegister rb
assignRegistersToInstruction (CSET a c) = CSET <$> outputRegister a <*> pure c
assignRegistersToInstruction (MOV rd ra) = MOV <$> outputRegister rd <*> inputRegister ra
assignRegistersToInstruction (MOVK rd ra) = MOVK <$> outputRegister rd <*> inputRegister ra
assignRegistersToInstruction b@BranchConditional{} = return b
assignRegistersToInstruction b@BranchAlways{} = return b
assignRegistersToInstruction b@BranchLink{} = return b
assignRegistersToInstruction (LDR rd (Address reg offset)) = LDR <$> outputRegister rd <*> (Address <$> inputRegister reg <*> pure offset)
assignRegistersToInstruction (STR rd (Address reg offset)) = STR <$> inputRegister rd <*> (Address <$> inputRegister reg <*> pure offset)
assignRegistersToInstruction (STP rd rs (Address reg offset)) = STP <$> inputRegister rd <*> inputRegister rs <*> (Address <$> inputRegister reg <*> pure offset)
assignRegistersToInstruction (ADRP rd label) = ADRP <$> outputRegister rd <*> pure label
assignRegistersToInstruction (AddNamedOffset rd ra label) = AddNamedOffset <$> outputRegister rd <*> inputRegister ra <*> pure label
assignRegistersToInstruction RET = return RET
assignRegistersToInstruction NOP = return NOP
assignRegistersToInstruction c@Comment{} = return c
assignRegistersToInstruction l@Label{} = return l
assignRegistersToInstruction (BasicBlock instrs) = do
    -- todo: make sure spill code is generated in the right place.
    let annotatedInstrs = annotateInstructions instrs
    -- get the currently in-use registers and registers from block above (parent)
    -- we need the parent's registers to make sure we don't throw them out in this block
    currentRegisters <- gets usedRegisters
    parentRegs <- gets parentRegisters
    modify $ \st -> st{parentRegisters = currentRegisters }

    -- assign registers in this block
    assignedInstrs <- assignRegisters annotatedInstrs

    -- put back parent state
    modify $ \st -> st{parentRegisters = parentRegs }

    -- clean up useless instructions and return the block
    return $ BasicBlock (cleanupInstructions assignedInstrs)


cleanupInstructions :: [AArch64Instruction] -> [AArch64Instruction]
cleanupInstructions [] = []
cleanupInstructions (m@(MOV a b) : xs)
    | a == b = cleanupInstructions xs
    | otherwise = m : cleanupInstructions xs
cleanupInstructions (NOP : xs) = cleanupInstructions xs
cleanupInstructions (x:xs) = x : cleanupInstructions xs
