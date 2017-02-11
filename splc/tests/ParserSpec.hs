module ParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec

import SplcParser

spec :: Spec
spec = do
    describe "The `int` integer parser" $ do
        it "parses 3" $
            parseInt "3" `shouldParse` (3 :: Integer)
        it "parses -3" $
            parseInt "-3" `shouldParse` (-3 :: Integer)
        it "parses arbitrary numbers" $
            property $ \x -> parseInt (show x) `shouldParse` (x :: Integer)
    describe "identifier" $ do
        it "parses single characters" $
            parseIdentifier "x" `shouldParse` "x"
        it "parses single uppercase characters" $
            parseIdentifier "X" `shouldParse` "X"
        it "doesn't parse strings starting with integers" $
            parseIdentifier `shouldFailOn` "3x"
        it "doesn't parse strings starting with underscore" $
            parseIdentifier `shouldFailOn` "_x"
        it "parses strings with later underscores" $
            parseIdentifier "x_x" `shouldParse` "x_x"
        it "parses strings with later numbers" $
            parseIdentifier "x3x" `shouldParse` "x3x"
        it "should not parse keywords" $ property $
            forAll (elements reserved) $ \x -> parseIdentifier `shouldFailOn` x
        it "should parse words" $
            parseIdentifier "test" `shouldParse` "test"
    where
        parseIdentifier = parse identifier ""
        parseInt = parse int ""
