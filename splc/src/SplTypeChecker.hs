{-# LANGUAGE FlexibleInstances #-}
module SplTypeChecker where

import qualified Data.Map as Map

import Prelude hiding (fail)
import Data.List
import Control.Monad (when)
import Control.Monad.Fail
import Control.Monad.Trans.State
import SplAST

-- Silly Haskell didn't make Either a MonadFail, let's do it anyway
instance MonadFail (Either String) where
    fail s = Left s

type Environment = Map.Map String SplTypeR
type SplTypeCheckResult = StateT Environment (Either String) SplTypeR

data SplSimpleTypeR = SplTypeConst SplBasicType
                    | SplTypeTupleR SplSimpleTypeR SplSimpleTypeR
                    | SplTypeListR SplSimpleTypeR
                    | SplTypeVar Int
                    | SplVoid
        deriving (Show, Eq)

data SplTypeR = SplSimple SplSimpleTypeR
              | SplTypeFunction [SplSimpleTypeR] SplSimpleTypeR
        deriving (Show, Eq)

emptyEnvironment :: Environment
emptyEnvironment = Map.empty

class SplTypeChecker a where
    typeCheck :: a -> SplTypeCheckResult

unsimple :: SplTypeR -> StateT Environment (Either String) SplSimpleTypeR
unsimple (SplSimple t) = return t
unsimple (SplTypeFunction _ _) = fail "Unexpected function type"

returnSimple :: SplSimpleTypeR -> SplTypeCheckResult
returnSimple x = return (SplSimple x)

disallowVoid :: SplSimpleTypeR -> StateT Environment (Either String) SplSimpleTypeR
disallowVoid (SplVoid) = fail "Void is not allowed"
disallowVoid t = return t

notVoid :: SplSimpleTypeR -> Bool
notVoid SplVoid = False
notVoid _       = True

instance SplTypeChecker SplType where
    typeCheck SplTypeUnknown = fail "Not allowed yet"
    typeCheck (SplTypePlaceholder _) = fail "Not allowed yet"
    typeCheck (SplTypeTuple l r) = do
        lt <- typeCheck l >>= unsimple >>= disallowVoid
        rt <- typeCheck r >>= unsimple >>= disallowVoid
        returnSimple (SplTypeTupleR lt rt)
    typeCheck (SplTypeList a) = do
        at <- typeCheck a >>= unsimple >>= disallowVoid
        returnSimple (SplTypeListR at)
    typeCheck (SplType t) = returnSimple $ SplTypeConst t


instance SplTypeChecker SplDecl where
    typeCheck (SplDeclVar varDecl) = typeCheck varDecl

    typeCheck (SplDeclFun name argNames argTypes retType varDecls stmts) = do
        declared <- gets (Map.member name)
        when declared
            (fail $ "Variable " ++ name ++ " already declared")

        -- TODO check for duplicate names

        checkedArgTypes <- mapM (\x -> typeCheck x >>= unsimple) argTypes
        localState <- get
        globalState <- get

        let nameArgs = zip argNames (map SplSimple checkedArgTypes)
        let localState' = foldr (\(s, n) -> Map.insert s n) localState nameArgs
        put localState'

        mapM_ (\x -> typeCheck x >>= unsimple) varDecls

        retType' <- case retType of
            SplRetType t ->  typeCheck t >>= unsimple
            SplRetVoid -> return SplVoid

        blockType <- typeCheck stmts >>= unsimple

        when (blockType /= retType') $
            fail $ "Cannot unify return type " ++ (show retType) ++
                " with actual type " ++ (show blockType) ++ "."

        put globalState
        return $ SplTypeFunction checkedArgTypes retType'


instance SplTypeChecker SplVarDecl where
    typeCheck (SplVarDecl varType name expr) = do
        t <- typeCheck varType
        e <- typeCheck expr
        _ <- unsimple e >>= disallowVoid
        if (t == e)
            then (do
                declared <- gets (Map.member name)
                when declared
                    (fail $ "Variable " ++ name ++ " already declared")
                modify (Map.insert name t)
                return t
            )
            else fail "Type mismatch when parsing varDecl"

typeCheckField :: SplSimpleTypeR -> SplField -> SplTypeCheckResult
typeCheckField t SplFieldNone = returnSimple t
typeCheckField (SplTypeListR t) (SplFieldHd f) = typeCheckField t f
typeCheckField t@(SplTypeListR _) (SplFieldTl f) = typeCheckField t f
typeCheckField (SplTypeTupleR l _) (SplFieldFst f) = typeCheckField l f
typeCheckField (SplTypeTupleR _ r) (SplFieldSnd f) = typeCheckField r f
typeCheckField _ (SplFieldHd _) = fail "Cannot take hd of a non-list"
typeCheckField _ (SplFieldTl _) = fail "Cannot take tl of a non-list"
typeCheckField _ (SplFieldFst _) = fail "Cannot take fst of a non-tuple"
typeCheckField _ (SplFieldSnd _) = fail "Cannot take snd of a non-tuple"

instance SplTypeChecker [SplStmt] where
    typeCheck [] = returnSimple SplVoid
    typeCheck (x:xs) = do
        xType <- typeCheck x >>= unsimple
        xsType <- typeCheck xs >>= unsimple
        case xType of
            SplVoid -> returnSimple xsType
            t1 -> case xsType of
                    SplVoid -> returnSimple xType
                    t2 -> if (t1 == t2)
                        then returnSimple t1
                        else fail $ "Different return types: " ++
                            (show t1) ++ " and " ++ (show t2) ++ "."


instance SplTypeChecker SplStmt where
    typeCheck (SplIfStmt cond thenStmts elseStmts) = do
        condType <- typeCheck cond >>= unsimple
        when (condType /= SplTypeConst SplBool) $
            fail $ "Condition needs to be a Bool instead of a " ++ (show condType)
        typeCheck (thenStmts ++ elseStmts)

    typeCheck (SplWhileStmt cond stmts) = do
        condType <- typeCheck cond >>= unsimple
        when (condType /= SplTypeConst SplBool) $
            fail $ "Condition needs to be a Bool instead of a " ++ (show condType)
        typeCheck stmts

    typeCheck (SplAssignmentStmt name field expr) = do
        varType' <- gets (Map.lookup name)
        varType <- case varType' of
            Just t -> unsimple t >>= (flip typeCheckField field) >>= unsimple
            Nothing -> fail $ "Undeclared variable " ++ name
        exprType <- typeCheck expr >>= unsimple >>= disallowVoid
        if (exprType == varType)
            then returnSimple SplVoid
            else fail $ "Cannot assign result of type " ++ (show exprType) ++
                    " to variable of type " ++ (show varType) ++ "."

    typeCheck (SplFuncCallStmt name args) = checkFunctionCall name args

    typeCheck (SplReturnStmt e) =
            typeCheck e >>= unsimple >>= disallowVoid >>= returnSimple

    typeCheck SplReturnVoidStmt = returnSimple SplVoid


instance SplTypeChecker SplExpr where
    typeCheck (SplIntLiteralExpr _) = returnSimple $ SplTypeConst SplInt
    typeCheck (SplBooleanLiteralExpr _) = returnSimple $ SplTypeConst SplBool
    typeCheck (SplCharLiteralExpr _) = returnSimple $ SplTypeConst SplChar
    typeCheck SplEmptyListExpr = fail "empty lists are weird without inference"

    typeCheck (SplTupleExpr l r) = do
            lType <- typeCheck l >>= unsimple >>= disallowVoid
            rType <- typeCheck r >>= unsimple >>= disallowVoid
            -- FIXME functions in tuples
            returnSimple $ SplTypeTupleR lType rType

    typeCheck (SplIdentifierExpr name field) = do
        varType <- gets (Map.lookup name)
        -- todo check if it's a function
        case varType of
            Just t -> unsimple t >>= (flip typeCheckField field)
            Nothing -> fail $ "Undeclared variable " ++ name

    typeCheck (SplUnaryExpr op expr) = do
        exprType <- typeCheck expr >>= unsimple >>= disallowVoid
        case op of
            SplOperatorInvert -> if (exprType == SplTypeConst SplBool)
                then returnSimple exprType
                else fail "Cannot invert a non-Bool"
            SplOperatorNegate -> if (exprType == SplTypeConst SplInt)
                then returnSimple exprType
                else fail "Cannot invert a non-Int"

    typeCheck (SplBinaryExpr SplOperatorCons e1 e2) = do
        e1Type <- typeCheck e1 >>= unsimple >>= disallowVoid
        e2Type <- typeCheck e2 >>= unsimple >>= disallowVoid
        case e2Type of
            SplTypeListR t2 -> if (e1Type == t2)
                then returnSimple e2Type
                else fail $ "Expected list of type " ++ (show e1Type)
            _ -> fail "Cons expects a list as second argument"

    typeCheck (SplBinaryExpr op e1 e2) = do
        e1Type <- typeCheck e1 >>= unsimple >>= disallowVoid
        e2Type <- typeCheck e2 >>= unsimple >>= disallowVoid
        when (e1Type /= e2Type) (
            fail $ "Both operands need to be of the same type\n"
                ++ "Actual: " ++ (show e1Type) ++ " " ++ (show op)
                ++ " " ++ (show e2Type))
        typeCheckBinOp op e1Type

    typeCheck (SplFuncCallExpr name args) = checkFunctionCall name args


checkFunctionCall :: String -> [SplExpr] -> SplTypeCheckResult
checkFunctionCall name args = do
    funcType <- gets (Map.lookup name)
    case funcType of
        Nothing -> fail $ "Undeclared function name " ++ name
        Just (SplSimple _) -> fail "Expected function, got variable"
        Just (SplTypeFunction argTypes retType) -> (do
                when (length args /= length argTypes) (
                        fail ("Got " ++ (show . length) args ++
                              " arguments but expected " ++ (show . length) argTypes))
                argTypeResults <- mapM typeCheck args
                let result = all checkArgType (zip argTypes argTypeResults)
                if result
                    then returnSimple retType
                    else fail ("Error checking arguments for " ++ name ++ ":\n"
                                 ++ "Expected: " ++ (intercalate ", " (map show args)) ++ "\n"
                                 ++ "Got:      " ++ (intercalate ", " (map show argTypes)))

            )
    where
        checkArgType :: (SplSimpleTypeR, SplTypeR) -> Bool
        checkArgType (expected, SplSimple actual) = actual == expected && notVoid actual
        checkArgType _ = False

typeCheckBinOp :: SplBinaryOperator -> SplSimpleTypeR -> SplTypeCheckResult
typeCheckBinOp SplOperatorAdd t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorAdd t@(SplTypeConst SplChar) = returnSimple t

typeCheckBinOp SplOperatorSubtract t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorSubtract t@(SplTypeConst SplChar) = returnSimple t

typeCheckBinOp SplOperatorMultiply t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorDivide t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorModulus t@(SplTypeConst SplInt) = returnSimple t

typeCheckBinOp SplOperatorLess t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorLess t@(SplTypeConst SplChar) = returnSimple t

typeCheckBinOp SplOperatorLessEqual t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorLessEqual t@(SplTypeConst SplChar) = returnSimple t

typeCheckBinOp SplOperatorEqual t@(SplTypeConst _) = returnSimple t
typeCheckBinOp SplOperatorEqual t@(SplTypeTupleR _ _) = returnSimple t
typeCheckBinOp SplOperatorEqual t@(SplTypeListR _) = returnSimple t

typeCheckBinOp SplOperatorGreaterEqual t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorGreaterEqual t@(SplTypeConst SplChar) = returnSimple t

typeCheckBinOp SplOperatorGreater t@(SplTypeConst SplInt) = returnSimple t
typeCheckBinOp SplOperatorGreater t@(SplTypeConst SplChar) = returnSimple t

typeCheckBinOp SplOperatorNotEqual t@(SplTypeConst _) = returnSimple t
typeCheckBinOp SplOperatorNotEqual t@(SplTypeTupleR _ _) = returnSimple t
typeCheckBinOp SplOperatorNotEqual t@(SplTypeListR _) = returnSimple t

typeCheckBinOp SplOperatorAnd t@(SplTypeConst SplBool) = returnSimple t
typeCheckBinOp SplOperatorOr t@(SplTypeConst SplBool) = returnSimple t

typeCheckBinOp SplOperatorCons _ = fail "Don't use typeCheckBinOp on Cons, dummy"
typeCheckBinOp o t = fail $ "Unsupported operands of type " ++ (show t) ++ " for operator " ++ (show o)
