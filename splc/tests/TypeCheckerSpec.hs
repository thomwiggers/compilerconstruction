module TypeCheckerSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except

import SplAST
import SplParser
import SplTypeChecker
import Text.Megaparsec (parse)

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
            (SplVarDecl (SplType SplInt) "name" (SplIntLiteralExpr 42)) `checksAs` voidType
        it "checks as Char" $
            "Char n = '🎉';" `parseChecksAs` voidType
        it "updates the environment" $
            (SplVarDecl (SplType SplInt) "name" (SplIntLiteralExpr 42)) `updatesStateWith` ((Var, "name"), Forall [] (SplSimple $ SplTypeConst SplInt))
        it "Disallows faulty declarations" $
            (SplVarDecl (SplType SplInt) "name" (SplCharLiteralExpr '🎉')) `failsWith` (UnificationFail (SplSimple $ SplTypeConst SplInt) (SplSimple $ SplTypeConst SplChar))
        describe "var n = '🎉'" $ do
            it "checks correctly" $
                "var n = '🎉';" `parseChecksAs` voidType
            it "updates the state correctly" $
                "var n = '🎉';" `parseUpdatesStateWith` ((Var, "n"), Forall [] (SplSimple $ SplTypeConst SplChar))
    describe "if statements" $ do
        it "checks correctly" $
            (SplIfStmt (SplBooleanLiteralExpr True) [] []) `checksAs` voidType
        describe "handle a return statement correctly" $ do
            it "return void" $
                checkInFunctionAs (SplIfStmt (SplBooleanLiteralExpr True) [SplReturnVoidStmt] [])
            -- FIXME
            it "return 42" $
                checkInFunctionAs (SplIfStmt (SplBooleanLiteralExpr True) [SplReturnStmt (SplIntLiteralExpr 42)] [])
        describe "multiple return statements" $ do
            it "return void" $
                checkInFunctionAs (SplIfStmt (SplBooleanLiteralExpr True) [SplReturnVoidStmt] [SplReturnVoidStmt])
            it "return 42; return 42" $
                checkInFunctionAs (SplIfStmt (SplBooleanLiteralExpr True) [SplReturnStmt (SplIntLiteralExpr 42)] [SplReturnStmt (SplIntLiteralExpr 42)])
            it "if (True) {return;} else {return 42;}" $
                (SplIfStmt (SplBooleanLiteralExpr True) [SplReturnVoidStmt] [SplReturnStmt $ SplIntLiteralExpr 42])
                    `failsInFunctionWith` (UnificationFail (SplSimple $ SplTypeConst SplInt) voidType)
            it "return 'c'; return 42" $
                (SplIfStmt (SplBooleanLiteralExpr True) [SplReturnStmt $ SplCharLiteralExpr 'c'] [SplReturnStmt $ SplIntLiteralExpr 42])
                    `failsInFunctionWith` (UnificationFail (SplSimple $ SplTypeConst SplInt) (SplSimple $ SplTypeConst SplChar))
    where
        checkInFunctionAs a = ((runInferWithState funEnv) . infer) a `shouldBe` (Right (Forall [] voidType))
        failsInFunctionWith a e = ((runInferWithState funEnv) . infer) a `shouldBe` (Left e)
        funEnv = SplEnv {nextFreshVar = 1, typeEnv = emptyTypeEnv, returnBlocks = [False], returnType = SplTypeVar $ TV $ head letters}
        checksAs a b = (runInfer . infer) a `shouldBe` (Right (Forall [] b))
        failsWith a e = (runInfer . infer) a `shouldBe` (Left e)
        parseChecksAs a b =
            case (parse spl "" a) of
                Right res -> res `checksAs` b
                Left err -> expectationFailure (show err)

        {- Check if the state contains what's expected -}
        updatesStateWith a (n, b) = do
            let (SplEnv {typeEnv=TypeEnv env}) = (execInfer . infer) a
            (Map.lookup n env) `shouldBe` (Just b)

        parseUpdatesStateWith a b = do
            case (parse spl "" a) of
                Right res -> res `updatesStateWith` b
                Left err -> expectationFailure (show err)

        runInferWithState :: SplEnv -> Infer (Subst, SplTypeR) -> Either TypeError Scheme
        runInferWithState env m = case evalState (runExceptT m) env of
            Left err -> Left err
            Right res -> Right $ closeOver res

voidType :: SplTypeR
voidType = SplSimple SplVoid
