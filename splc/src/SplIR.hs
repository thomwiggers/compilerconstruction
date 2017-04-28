module SplIR where

import Prelude
import SplAST
import Control.Monad.State

type SplIR = [SplInstruction]

type SplPseudoRegister = String
type SplLabel = String

data SplOperation
    = SplAdd
    | SplDec
    | SplCmp
    deriving (Show)

data SplInstruction
    = SplFunction SplLabel [SplPseudoRegister] SplIR
    | SplOperation SplOperation SplPseudoRegister SplPseudoRegister SplPseudoRegister
    | SplJumpTarget SplLabel
    | SplRet SplPseudoRegister
    | SplJump SplLabel
    | SplJumpIf SplPseudoRegister SplLabel
    | SplJumpIfNot SplPseudoRegister SplLabel
    | SplMov SplLabel SplLabel
    | SplCall SplLabel [SplPseudoRegister]
    deriving (Show)


data Env = Env {
        nextVar :: Int,
        nextLabel :: Int
    }

type IRState = State Env


getNextVar :: IRState String
getNextVar = do
    s <- get
    let i = nextVar s
    put s{nextVar = 1 + (nextVar s)}
    return $ "t" ++ show i

getNextLabel :: String -> IRState String
getNextLabel prefix = do
    s <- get
    let i = nextLabel s
    put s{nextLabel = 1 + (nextLabel s)}
    return $ prefix ++ show i

exprToIR :: SplExpr -> IRState (SplIR, SplPseudoRegister)
exprToIR (SplIdentifierExpr name SplFieldNone) = return ([], name)

replaceName :: SplPseudoRegister -> SplPseudoRegister -> SplInstruction -> SplInstruction
replaceName from to instruction = case instruction of
    (SplCall label args) -> SplCall label $ map replace args
    (SplOperation op target a b) -> SplOperation op (replace target) (replace a) (replace b)
    (SplRet r) -> SplRet (replace r)
    (SplFunction label args ir) -> SplFunction label (map replace args) ir
    (SplJumpIf r label) -> SplJumpIf (replace r) label
    (SplJumpIfNot r label) -> SplJumpIfNot (replace r) label
    (SplMov r r2) -> SplMov (replace r) (replace r2)
    SplJumpTarget t -> SplJumpTarget t
    SplJump t -> SplJump t
    where
    replace x = if (x == from) then to else x

toIR :: SplStmt -> IRState SplIR
toIR (SplWhileStmt cond loop) = do
    (condIR, compareRegister) <- exprToIR cond
    label <- getNextLabel "while"
    let labelAfter = label ++ "end"
    loopIR <- mapM toIR loop
    return $ (SplJumpTarget label : condIR) ++
            (SplJumpIfNot compareRegister labelAfter : (concat loopIR) ++ [SplJump label])

toIR (SplIfStmt cond thenStmts elseStmts) = do
    (condIR, compareRegister) <- exprToIR cond
    elseLabel <- getNextLabel "else"
    let elseAfterLabel = elseLabel ++ "end"
    thenIR <- mapM toIR thenStmts
    elseIR <- mapM toIR elseStmts
    return $ condIR ++ (SplJumpIfNot compareRegister elseLabel : (concat thenIR ++ [SplJump elseAfterLabel]))
        ++ (SplJumpTarget elseLabel : concat elseIR) ++ [SplJumpTarget elseAfterLabel]

toIR (SplAssignmentStmt name SplFieldNone expr) = do
    (condIR, resultRegister) <- exprToIR expr
    let condIR' = map (replaceName resultRegister name) condIR
    return condIR'

toIR (SplFuncCallStmt name args) = do
    argIRsResultRegs <- mapM exprToIR args
    let (argIRs, resultRegs) = unzip argIRsResultRegs
    return (concat argIRs ++ [SplCall name resultRegs])
