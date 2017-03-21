{-# LANGUAGE FlexibleInstances #-}
module SplTypeChecker where

import qualified Data.Map as Map

import Prelude hiding (fail)
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
        deriving (Show, Eq)

data SplReturnTypeR = SplRetTypeR SplSimpleTypeR
                    | SplRetVoidR
        deriving (Show, Eq)

data SplTypeR = SplSimple SplSimpleTypeR
              | SplTypeFunction [SplSimpleTypeR] SplReturnTypeR
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

instance SplTypeChecker SplType where
    typeCheck SplTypeUnknown = fail "Not allowed yet"
    typeCheck (SplTypePlaceholder _) = fail "Not allowed yet"
    typeCheck (SplTypeTuple l r) = do
        lt <- typeCheck l >>= unsimple
        rt <- typeCheck r >>= unsimple
        returnSimple (SplTypeTupleR lt rt)
    typeCheck (SplTypeList a) = do
        at <- typeCheck a >>= unsimple
        returnSimple (SplTypeListR at)
    typeCheck (SplType t) = returnSimple $ SplTypeConst t

instance SplTypeChecker SplVarDecl where
    typeCheck (SplVarDecl varType name expr) = do
        t <- typeCheck varType
        e <- typeCheck expr
        if (t == e) 
            then (do
                declared <- gets (Map.member name)
                when declared
                    (fail $ "Variable " ++ name ++ " already declared")
                modify (Map.insert name t)
                return t
            )
            else fail "Type mismatch when parsing varDecl"

instance SplTypeChecker SplExpr where
    typeCheck (SplIntLiteralExpr _) = returnSimple $ SplTypeConst SplInt
    typeCheck (SplBooleanLiteralExpr _) = returnSimple $ SplTypeConst SplBool
    typeCheck (SplCharLiteralExpr _) = returnSimple $ SplTypeConst SplChar
    typeCheck SplEmptyListExpr = fail "huh"
    typeCheck (SplIdentifierExpr name SplFieldNone) = do
        varType <- gets (Map.lookup name)
        case varType of
            Just t -> return t
            Nothing -> fail $ "Undeclared variable " ++ name
    typeCheck (SplFuncCallExpr name args) = do
        funcType <- gets (Map.lookup name)
        case funcType of
            Just t -> return t
            Nothing -> fail $ "Undeclared variable " ++ name

    typeCheck _ = fail "Not implemented"
