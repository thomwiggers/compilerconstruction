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
    describe "bool" $ do
        it "parses True" $ do
            parseBool "True" `shouldParse` True
        it "parses False" $ do
            parseBool "False" `shouldParse` False
        it "doesn't parse anything else" $ property $
            \x -> not (x `elem` ["True", "False"]) ==> parseBool `shouldFailOn` x
    describe "char" $ do
        it "parses 'a'" $
            parseChar "'a'" `shouldParse` 'a'
        it "parses arbitrary simple chars" $ property $
            \x -> x /= '\\' ==> parseChar ('\'':x:"'") `shouldParse` x
        it "parses some escapes" $ property $
            forAll (elements [('\\', '\\'), ('n', '\n'), ('r', '\r'), ('t', '\t'), ('\'', '\'')]) $
                \(x, y) -> parseChar ("'\\" ++ (x:"'") ) `shouldParse` y
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
    describe "parseType" $ do
        it "parses basic types" $ property $
            forAll (elements basicTypes) $
               \(x, y) -> parseType' x `shouldParse` (SplType y)
        it "parses tuples of basic types" $ property $
            forAll (elements [(a, b) | a <- basicTypes, b <- basicTypes]) $
               \((x, xt), (y, yt)) -> parseType' ("(" ++ x ++ ", " ++ y ++ ")") `shouldParse` (SplTypeTuple (SplType xt) (SplType yt))
    describe "sc" $ do
        it "eats whitespace" $
            parseSc "  " `succeedsLeaving` ""
        it "eats newlines" $
            parseSc "\n" `succeedsLeaving` ""
        it "eats carriage returns" $
            parseSc "\r" `succeedsLeaving` ""
        it "eats tabstops" $
            parseSc "\t\t" `succeedsLeaving` ""
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
    describe "expr" $ do
        it "parses trivial binary expressions" $ property $
            forAll (elements theBinaryOperators) $
                \(x, y) -> parseExpr ("1 " ++ x ++ " 1") `shouldParse` (SplBinaryExpr y literalOne literalOne)
        it "parses trivial binary expressions without spaces" $ property $
            forAll (elements theBinaryOperators) $
                \(x, y) -> parseExpr ("1" ++ x ++ "1") `shouldParse` (SplBinaryExpr y literalOne literalOne)
        it "parses trivial unary expressions" $ property $
            forAll (elements theUnaryOperators) $
                \(x, y) -> parseExpr (x ++ "1") `shouldParse` (SplUnaryExpr y literalOne)
        it "parses trivial unary expressions with spaces" $ property $
            forAll (elements theUnaryOperators) $
                \(x, y) -> parseExpr (x ++ "  1") `shouldParse` (SplUnaryExpr y literalOne)
        it "parses 1 + 1 + 1 left-associatively" $
            parseExpr "1 + 1 + 1" `shouldParse` (SplBinaryExpr SplOperatorAdd (SplBinaryExpr SplOperatorAdd literalOne literalOne) literalOne)
        it "parser '1 : 1 : 1' right-associatively" $
            parseExpr "1 : 1 : 1" `shouldParse` (SplBinaryExpr SplOperatorCons literalOne (SplBinaryExpr SplOperatorCons literalOne literalOne))
        it "parses 1 + 1 * 1 with the correct priority" $
            parseExpr "1 + 1 * 1" `shouldParse` (SplBinaryExpr SplOperatorAdd literalOne (SplBinaryExpr SplOperatorMultiply literalOne literalOne))
        it "parses (1 + 1) * 1 with the correct priority" $
            parseExpr "(1 + 1) * 1" `shouldParse` (SplBinaryExpr SplOperatorMultiply (SplBinaryExpr SplOperatorAdd literalOne literalOne) literalOne)
        it "Parses 'True && ~True'" $
            parseExpr "True && ~True" `shouldParse` (SplBinaryExpr SplOperatorAnd literalTrue (SplUnaryExpr SplOperatorInvert literalTrue))
        it "Parses 'True || True && True'" $
            parseExpr "True || True && True" `shouldParse` (SplBinaryExpr SplOperatorOr literalTrue (SplBinaryExpr SplOperatorAnd literalTrue literalTrue))
        it "doesn't parse half binary expressions (1 + )" $ property $
            forAll (elements theBinaryOperators) $
                \(x, _) -> parseExpr `shouldFailOn` ("1 " ++ x)
        it "doesn't parse half binary expressions (* 1)" $ property $
            forAll (elements theBinaryOperators) $
                \(x, _) -> not (x `elem` ["-", "~"]) ==> parseExpr `shouldFailOn` (x ++ " 1")
        it "parses Integer literals" $ property $
            -- > 0 because otherwise we get a unary expression with -
            \x -> x >=0 ==> parseExpr (show x) `shouldParse` (SplIntLiteralExpr x)
        it "parses Boolean literals" $ property $
            \x -> parseExpr (show x) `shouldParse` (SplBooleanLiteralExpr x)
        it "parses Character literals" $ property $
            \x -> parseExpr (show x) `shouldParse` (SplCharLiteralExpr x)
        it "parses Character literals like newline" $
            parseExpr "'\\n'" `shouldParse` (SplCharLiteralExpr '\n')
    describe "spl" $ do
        it "Does not parse empty files" $
            parseSpl `shouldFailOn` ""
        it "Parses a single untyped variable declaration" $
            parseSpl "var x = 3;" `shouldParse` (Spl [SplDeclVar ((SplVarDecl SplTypeUnknown) "x" (SplIntLiteralExpr 3))])
        it "Parses a function definition without args or types" $
            parseSpl "fun () { return 1; }" `shouldParse` (Spl [SplDeclFun "fun" [] [] (SplRetType SplTypeUnknown) [] [SplReturnStmt literalOne]])
    where
        literalOne = SplIntLiteralExpr 1
        literalTrue = SplBooleanLiteralExpr True
        parseSc x = runParser' sc (initialState x)
        parseIdentifier = parse identifier ""
        parseInt = parse int ""
        parseBool = parse bool ""
        parseChar = parse character ""
        parseBasicType = parse basicType ""
        parseType' = parse parseType ""
        parseSpl = parse spl ""
        parseExpr = parse expr ""
        theUnaryOperators = [
                ("~", SplOperatorInvert),
                ("-", SplOperatorNegate)
            ]
        theBinaryOperators = [
                ("+", SplOperatorAdd),
                ("-", SplOperatorSubtract),
                ("*", SplOperatorMultiply),
                ("/", SplOperatorDivide),
                ("%", SplOperatorModulus),
                ("<", SplOperatorLess),
                ("<=", SplOperatorLessEqual),
                ("==", SplOperatorEqual),
                (">=", SplOperatorGreaterEqual),
                (">", SplOperatorGreater),
                ("!=", SplOperatorNotEqual),
                ("&&", SplOperatorAnd),
                ("||", SplOperatorOr),
                (":", SplOperatorCons)
            ]
        basicTypes = [
                ("Bool", SplBool),
                ("Int", SplInt),
                ("Char", SplChar)
            ]
