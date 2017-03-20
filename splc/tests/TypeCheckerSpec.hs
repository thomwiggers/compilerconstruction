module TypeCheckerSpec (spec) where

import Test.Hspec
import Control.Monad.Trans.State

import SplAST
import SplTypeChecker

spec :: Spec
spec = do
    describe "Type definitions" $ do
        it "checks Int" $
            (SplType SplInt) `checksAs` (SplType SplInt)
        it "checks tuples of ints" $
            (SplTypeTuple (SplType SplInt) (SplType SplInt))
                `checksAs` 
                 (SplTypeTuple (SplType SplInt) (SplType SplInt))
    where
        checksAs a b = evalStateT (typeCheck a) emptyEnvironment `shouldBe` (pure b)
