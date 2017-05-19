module SplASTtoIR where

import           Control.Monad.State
import           Prelude
import           SplAST
import           SplIR

type IRState = State Env

data Env = Env {
        nextVar   :: Int,
        nextLabel :: Int
    }


astToIR :: Spl -> SplIR
astToIR spl = evalState (toIR spl) Env{nextVar = -1, nextLabel = -1}

getNextVar :: IRState SplPseudoRegister
getNextVar = do
    s <- get
    let i = nextVar s
    put s{nextVar = 0 + nextVar s}
    return $ Reg $ "t" ++ show i

getNextLabel :: String -> IRState String
getNextLabel prefix = do
    s <- get
    let i = nextLabel s
    put s{nextLabel = 0 + nextLabel s}
    return $ prefix ++ show i

exprToIR :: SplExpr -> IRState ([SplInstruction], SplPseudoRegister)
exprToIR (SplIdentifierExpr name field) = return ([], wrapField field $ Reg name)

exprToIR (SplBinaryExpr op e0 e1) = do
    (e0IR, e0ResultRegister) <- exprToIR e0
    (e1IR, e1ResultRegister) <- exprToIR e1
    resultRegister <- getNextVar
    return (e0IR ++ e1IR ++
                [SplBinaryOperation op resultRegister e0ResultRegister e1ResultRegister],
            resultRegister)

exprToIR (SplUnaryExpr op e0) = do
    (e0IR, e0ResultRegister) <- exprToIR e0
    resultRegister <- getNextVar
    return (e0IR ++
                [SplUnaryOperation op resultRegister e0ResultRegister],
            resultRegister)

exprToIR (SplTupleExpr l r) = do
    (lIR, lResultRegister) <- exprToIR l
    (rIR, rResultRegister) <- exprToIR r
    resultRegister <- getNextVar
    return (lIR ++ rIR ++ [SplMov (TupleFst resultRegister) lResultRegister,
                           SplMov (TupleSnd resultRegister) rResultRegister],
            resultRegister)

exprToIR (SplIntLiteralExpr i) = do
    resultRegister <- getNextVar
    return ([SplMovImm resultRegister (SplImmInt i)], resultRegister)

exprToIR (SplCharLiteralExpr i) = do
    resultRegister <- getNextVar
    return ([SplMovImm resultRegister (SplImmChar i)], resultRegister)

exprToIR (SplBooleanLiteralExpr i) = do
    resultRegister <- getNextVar
    return ([SplMovImm resultRegister (SplImmBool i)], resultRegister)

exprToIR (SplFuncCallExpr name args) = do
    argIRsResultRegs <- mapM exprToIR args
    let (argIRs, argResultRegs) = unzip argIRsResultRegs
    resultRegister <- getNextVar
    return (concat argIRs ++ [SplCall name (Just resultRegister) argResultRegs],
            resultRegister)

exprToIR SplEmptyListExpr = do
    resultRegister <- getNextVar
    return ([SplMovImm resultRegister (SplImmInt $ -1)], resultRegister)

wrapField :: SplField -> SplPseudoRegister -> SplPseudoRegister
wrapField SplFieldNone inner    = inner
wrapField (SplFieldFst f) inner = wrapField f $ TupleFst inner
wrapField (SplFieldSnd f) inner = wrapField f $ TupleSnd inner
wrapField (SplFieldHd f)  inner = wrapField f $ ListHd inner
wrapField (SplFieldTl f)  inner = wrapField f $ ListTl inner

replaceName :: SplPseudoRegister -> SplPseudoRegister -> SplInstruction -> SplInstruction
replaceName from to instruction = case instruction of
    SplCall label Nothing args -> SplCall label Nothing $ map replace args
    SplCall label (Just resultRegister) args -> SplCall label (Just $ replace resultRegister) $ map replace args
    SplBinaryOperation op target a b -> SplBinaryOperation op (replace target) (replace a) (replace b)
    SplUnaryOperation op target a -> SplUnaryOperation op (replace target) (replace a)
    SplRet (Just r) -> SplRet $ Just (replace r)
    SplRet Nothing -> SplRet Nothing
    SplFunction label args ir -> SplFunction label (map replace args) ir
    SplMov r r1 -> SplMov (replace r) (replace r1)
    SplMovImm r i -> SplMovImm (replace r) i
    w@SplWhile{} -> w
    i@SplIf{} -> i
    where
    replace x = if x == from then to else x

class ToIR a where
    toIR :: a -> IRState SplIR

instance ToIR SplStmt where
    toIR (SplWhileStmt cond loop) = do
        condIR <- exprToIR cond
        label <- getNextLabel "while"
        loopIR <- concat <$> mapM toIR loop
        return [SplWhile label condIR loopIR]

    toIR (SplIfStmt cond thenStmts elseStmts) = do
        (condIR, compareRegister) <- exprToIR cond
        label <- getNextLabel "if"
        thenIR <- concat <$> mapM toIR thenStmts
        elseIR <- concat <$> mapM toIR elseStmts
        return $ condIR ++ [SplIf label compareRegister thenIR elseIR]

    toIR (SplAssignmentStmt name field expr) = do
        (exprIR, resultRegister) <- exprToIR expr
        let wrappedReg = wrapField field (Reg name)
        let exprIR' = map (replaceName resultRegister wrappedReg) exprIR
        return exprIR'

    toIR (SplFuncCallStmt name args) = do
        (argIRs, resultRegs) <- unzip <$> mapM exprToIR args
        return (concat argIRs ++ [SplCall name Nothing resultRegs])

    toIR SplReturnVoidStmt = return [SplRet Nothing]

    toIR (SplReturnStmt expr) = do
        (exprIR, resultRegister) <- exprToIR expr
        return $ exprIR ++ [SplRet (Just resultRegister)]

instance ToIR Spl where
    toIR (Spl x) = do
        z <- mapM toIR x
        return $ concat z

instance ToIR SplDecl where
    toIR (SplDeclVar (SplVarDecl _ name expr)) = do
        (eIR, eReg) <- exprToIR expr
        return $ eIR ++ [SplMov (Reg name) eReg]

    toIR (SplDeclFun name argnames _ _ decls stmts) = do
        declIR <- concat <$> mapM toIR decls
        stmtIR <- concat <$> mapM toIR stmts
        return [SplFunction name (map Reg argnames) (declIR ++ stmtIR)]

instance ToIR SplVarDecl where
    toIR (SplVarDecl _ name expr) = do
        (eIR, resultReg) <- exprToIR expr
        let declIR = map (replaceName resultReg (Reg name)) eIR
        return declIR
