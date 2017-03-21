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

type Environment = Map.Map String SplType
type SplTypeCheckResult = StateT Environment (Either String) SplType

emptyEnvironment :: Environment
emptyEnvironment = Map.empty

class SplTypeChecker a where
    typeCheck :: a -> SplTypeCheckResult

instance SplTypeChecker SplType where
    typeCheck SplTypeUnknown = fail "Not allowed yet"
    typeCheck (SplTypePlaceholder _) = fail "Not allowed yet"
    typeCheck (SplTypeTuple l r) = SplTypeTuple <$> typeCheck l <*> typeCheck r
    typeCheck (SplTypeList a) = SplTypeList <$> typeCheck a
    typeCheck t@(SplType _) = pure t

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
    typeCheck (SplIntLiteralExpr _) = return $ SplType SplInt
    typeCheck (SplBooleanLiteralExpr _) = return $ SplType SplBool
    typeCheck (SplCharLiteralExpr _) = return $ SplType SplChar
    typeCheck SplEmptyListExpr = fail "huh"
    typeCheck (SplIdentifierExpr name SplFieldNone) = do
        varType <- gets (Map.lookup name)
        case varType of
            Just t -> return t
            Nothing -> fail $ "Undeclared variable " ++ name
    typeCheck _ = fail "Not implemented"
