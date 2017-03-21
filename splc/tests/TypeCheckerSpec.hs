module TypeCheckerSpec (spec) where

import Test.Hspec
import Control.Monad.Trans.State
import qualified Data.Map as Map

import SplAST
import SplTypeChecker

spec :: Spec
spec = do
    describe "Type definitions" $ do
        it "checks Int" $
            (SplType SplInt) `checksAs` (SplSimple $ SplTypeConst SplInt)
        it "checks tuples of ints" $
            (SplTypeTuple (SplType SplInt) (SplType SplInt))
                `checksAs` 
                 (SplSimple (SplTypeTupleR (SplTypeConst SplInt) (SplTypeConst SplInt)))
    describe "Variable Declarations" $ do
        it "checks as Int" $
            (SplVarDecl (SplType SplInt) "name" (SplIntLiteralExpr 42)) `checksAs` (SplSimple $ SplTypeConst SplInt)
        it "updates the environment" $
            (SplVarDecl (SplType SplInt) "name" (SplIntLiteralExpr 42)) `updatesStateWith` ("name", SplSimple $ SplTypeConst SplInt)
        it "Disallows faulty declarations" $
            (SplVarDecl (SplType SplInt) "name" (SplCharLiteralExpr '🎉')) `failsWith` "Type mismatch when parsing varDecl"
    where
        checksAs a b = evalStateT (typeCheck a) emptyEnvironment `shouldBe` (Right b)
        failsWith a e = runStateT (typeCheck a) emptyEnvironment `shouldBe` (Left e)

        {- Check if the state contains what's expected
         -
         - Function eliminates Either from the state, then the lookup may have a Just.
         -}
        updatesStateWith :: (SplTypeChecker a) => a -> (String, SplTypeR) -> Expectation
        updatesStateWith a (n, b) = do
            let eitherenv = execStateT (typeCheck a) emptyEnvironment
            case eitherenv of
                Right env -> shouldBe (Map.lookup n env) (Just b)
                Left e -> expectationFailure e
