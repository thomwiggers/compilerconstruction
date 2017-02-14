module ParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec

import SplParser

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
    describe "basicType" $ do
        it "parses Int" $
            parseBasicType "Int" `shouldParse` SplInt
        it "parses Char" $
            parseBasicType "Char" `shouldParse` SplChar
        it "parses Bool" $
            parseBasicType "Bool" `shouldParse` SplBool
        it "doesn't parse anything else" $ property $
            \x -> not (x `elem` ["Int", "Bool", "Char"]) ==> parseBasicType `shouldFailOn` x
    describe "unaryOperator" $ do
        it "parses invert" $
            parseUnaryOperator "!" `shouldParse` SplOperatorInvert
        it "parses negate" $
            parseUnaryOperator "-" `shouldParse` SplOperatorNegate
        it "doesn't parse anythign else" $ property $
            \x -> not (x `elem` ["-", "!"]) ==> parseBasicType `shouldFailOn` x
    describe "sc" $ do
        it "eats whitespace" $
            parseSc "  " `succeedsLeaving` ""
        it "eats newlines" $
            parseSc "\n" `succeedsLeaving` ""
        it "eats carriage returns" $
            parseSc "\r" `succeedsLeaving` ""
        it "eats crlf" $
            parseSc "\r" `succeedsLeaving` ""
        it "eats crlf" $
            parseSc "\r\n" `succeedsLeaving` ""
        it "eats line comments" $
            parseSc "// blablabla" `succeedsLeaving` ""
        it "eats line comments and leaves stuff after newline" $
            parseSc "// blablabla\ntest" `succeedsLeaving` "test"
        it "eats block comments" $
            parseSc "/* test */" `succeedsLeaving` ""
        it "eats block comments and preserves other stuff" $
            parseSc "/* test */follows" `succeedsLeaving` "follows"
    describe "spl" $ do
        it "Does not parse empty files" $
            parseSpl `shouldFailOn` ""
        it "Parses a single untyped variable declaration" $
            parseSpl "var x = 3;" `shouldParse` (Spl [SplDeclVar ((SplVarDecl SplTypeUnknown) "x" (SplIntLiteralExpr 3))])
    where
        parseSc x = runParser' sc (initialState x)
        parseIdentifier = parse identifier ""
        parseInt = parse int ""
        parseBasicType = parse basicType ""
        parseUnaryOperator = parse unaryOperator ""
        parseSpl = parse spl ""
