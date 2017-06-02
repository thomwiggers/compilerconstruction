module SplAArch64Allocator where

import qualified Data.Set as Set

import SplAArch64

-- instructions with the registers that are live *after* that point
type AnnotatedInstructions = [(AArch64Instruction, Set.Set Register)]

annotateInstructions :: [AArch64Instruction] -> AnnotatedInstructions
annotateInstructions z =
     map (\(i, rs) -> (i, Set.filter notImmediate rs)) $
        reverse $ annotate (reverse z) Set.empty
    where
        annotate [] _  = []
        annotate (x:xs) prev_live = (x, new_live) : annotate xs new_live
            where
                live_without_output = prev_live Set.\\ output x
                new_live = live_without_output `Set.union` inputs x

        notImmediate (Imm _) = False
        notImmediate _ = True


inputs :: AArch64Instruction -> Set.Set Register
inputs (ADD _ a b) = Set.fromList [a, b]
inputs (MUL _ a b) = Set.fromList [a, b]
inputs (SDIV _ a b) = Set.fromList [a, b]
inputs (SUB _ a b) = Set.fromList [a, b]
inputs (NEG _ a) = Set.singleton a
inputs (MSUB _ a b c) = Set.fromList [a, b, c]
inputs (AND _ a b) = Set.fromList [a, b]
inputs (OR _ a b) = Set.fromList [a, b]
inputs (XOR _ a b) = Set.fromList [a, b]
inputs (CMP a b) = Set.fromList [a, b]
inputs (MOV _ a) = Set.singleton a
inputs BranchConditional{} = Set.empty
inputs BranchAlways{} = Set.empty
inputs BranchLink{} = Set.empty
inputs (LDR _ (Address r _)) = Set.singleton r
inputs (STR r (Address a _)) = Set.fromList [r, a]
inputs RET = Set.empty
inputs NOP = Set.empty
inputs Comment{} = Set.empty
inputs Label{} = Set.empty
inputs (BasicBlock instrs) = head . map snd $ annotateInstructions instrs

output :: AArch64Instruction -> Set.Set Register
output (ADD o _ _) = Set.singleton o
output (MUL o _ _) = Set.singleton o
output (SDIV o _ _) = Set.singleton o
output (SUB o _ _) = Set.singleton o
output (NEG o _) = Set.singleton o
output (MSUB o _ _ _) = Set.singleton o
output (AND o _ _) = Set.singleton o
output (OR o _ _) = Set.singleton o
output (XOR o _ _) = Set.singleton o
output CMP{} = Set.empty
output (MOV o _) = Set.singleton o
output BranchConditional{} = Set.empty
output BranchAlways{} = Set.empty
output BranchLink{} = Set.empty
output (LDR r _) = Set.singleton r
output (STR _ _) = Set.empty
output RET = Set.empty
output NOP = Set.empty
output Comment{} = Set.empty
output Label{} = Set.empty
output (BasicBlock _) = Set.empty -- for now we assume this can be empty
