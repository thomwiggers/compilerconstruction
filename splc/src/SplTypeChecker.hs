module SplTypeChecker where

import qualified Data.Map as Map

import Control.Monad.Trans.State
import SplAST


type Environment = Map.Map String SplType
type SplTypeCheckResult = StateT Environment (Either String) SplType


emptyEnvironment :: Environment
emptyEnvironment = Map.empty

class SplTypeChecker a where
    typeCheck :: a -> SplTypeCheckResult

instance SplTypeChecker SplType where
    typeCheck SplTypeUnknown = fail "Not allowed yet"
    typeCheck (SplTypePlaceholder _) = fail "Not allowed yet"
    typeCheck t = pure t

instance SplTypeChecker SplVarDecl where
    typeCheck (SplVarDecl varType name expr) = do
        t <- typeCheck varType
        e <- typeCheck expr
        if (t == e) 
            then pure t
            else fail "Type mismatch when parsing varDecl"

instance SplTypeChecker SplExpr where
    typeCheck _ = fail "Not implemented"
