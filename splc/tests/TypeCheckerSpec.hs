module TypeCheckerSpec (spec) where

import Test.Hspec
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
            (SplVarDecl (SplType SplInt) "name" (SplIntLiteralExpr 42)) `checksAs` (voidType)
        it "updates the environment" $
            (SplVarDecl (SplType SplInt) "name" (SplIntLiteralExpr 42)) `updatesStateWith` ((Var, "name"), Forall [] (SplSimple $ SplTypeConst SplInt))
        it "Disallows faulty declarations" $
            (SplVarDecl (SplType SplInt) "name" (SplCharLiteralExpr '🎉')) `failsWith` (UnificationFail (SplSimple $ SplTypeConst SplInt) (SplSimple $ SplTypeConst SplChar))
    where
        checksAs a b = (runInfer . infer) a `shouldBe` (Right (Forall [] b))
        failsWith a e = (runInfer . infer) a `shouldBe` (Left e)

        {- Check if the state contains what's expected
         -
         - Function eliminates Either from the state, then the lookup may have a Just.
         -}
        updatesStateWith a (n, b) = do
            let (SplEnv {typeEnv=TypeEnv env}) = (execInfer . infer) a
            (Map.lookup n env) `shouldBe` (Just b)

voidType :: SplTypeR
voidType = SplSimple SplVoid
