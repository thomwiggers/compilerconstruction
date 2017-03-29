module ParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec

import SplParser
import SplAST

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
        it "parses lists of basic types" $ property $
            forAll (elements basicTypes) $
               \(x, y) -> parseType' ("[" ++ x ++ "]") `shouldParse` (SplTypeList (SplType y))
        it "parses lists of lists" $ property $
            forAll (elements basicTypes) $
               \(x, y) -> parseType' ("[[" ++ x ++ "]]") `shouldParse` (SplTypeList $ SplTypeList $ SplType y)
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
        it "parses ids"$
            parseExpr "id" `shouldParse` (SplIdentifierExpr "id" SplFieldNone)
        it "parses ids with Fields" $
            parseExpr "id.hd" `shouldParse` (SplIdentifierExpr "id" (SplFieldHd (SplFieldNone)))
        it "parses ids with complex Fields" $
            parseExpr "id.hd.tl" `shouldParse` (SplIdentifierExpr "id" (SplFieldHd $ SplFieldTl $ SplFieldNone))
        it "Parses variables in expressions" $
            parseExpr "a + 1" `shouldParse` (SplBinaryExpr SplOperatorAdd (SplIdentifierExpr "a" SplFieldNone) literalOne)
        it "Parses tuples" $
            parseExpr "(1, 1)" `shouldParse` (SplTupleExpr literalOne literalOne)
        it "Parses function calls" $
            parseExpr "foo()" `shouldParse` (SplFuncCallExpr "foo" [])
        it "Parses function calls with one arg" $
            parseExpr "foo(1)" `shouldParse` (SplFuncCallExpr "foo" [literalOne])
        it "Parses function calls with two args" $
            parseExpr "foo(1, 1)" `shouldParse` (SplFuncCallExpr "foo" [literalOne, literalOne])
        it "Parses nested parentheses" $
            parseExpr "(((((1)))))" `shouldParse` literalOne
    describe "spl" $ do
        it "Does not parse empty files" $
            parseSpl `shouldFailOn` ""
        it "Parses a single untyped variable declaration" $
            parseSpl "var x = 3;" `shouldParse`
                (Spl [SplDeclVar ((SplVarDecl SplTypeUnknown) "x" (SplIntLiteralExpr 3))])
        it "Parses a function definition without args or types" $
            parseSpl "fun () { return 1; }" `shouldParse`
                (Spl [SplDeclFun "fun" [] [] (SplRetType SplTypeUnknown) [] [SplReturnStmt literalOne]])
        it "Does parse a function with two statements" $
            parseSpl "fun () { return 1; return 1; }" `shouldParse`
                (Spl [SplDeclFun "fun" [] [] (SplRetType SplTypeUnknown)
                                [] [SplReturnStmt literalOne, SplReturnStmt literalOne]])
        it "Parses a function definition without args or types, with a var decl" $
            parseSpl "fun () { Int a = 1; return 1; }" `shouldParse`
                (Spl [SplDeclFun "fun" [] [] (SplRetType SplTypeUnknown)
                    [SplVarDecl (SplType SplInt) "a" literalOne] [SplReturnStmt literalOne]])
        it "Parses a function definition which updates a variable" $
            parseSpl "fun () { Int a = 1; a = a + 1; }" `shouldParse`
                (Spl [SplDeclFun "fun" [] [] (SplRetType SplTypeUnknown)
                    [SplVarDecl (SplType SplInt) "a" literalOne] [(SplAssignmentStmt "a" SplFieldNone (SplBinaryExpr SplOperatorAdd (SplIdentifierExpr "a" SplFieldNone) literalOne))]])
        it "Doesn't parse an invalid function without types" $
            parseSpl `shouldFailOn` "fun () :: { return 1; }"
        it "Doesn't parse a function with types but no arguments" $
            parseSpl `shouldFailOn` "fun () :: a -> Int { return 1; }"
        it "Does parse a function with types and the same number of arguments" $
            parseSpl "fun (a) :: a -> Int { return 1; }" `shouldParse`
                (Spl [SplDeclFun "fun" ["a"] [SplTypePlaceholder "a"]
                                (SplRetType (SplType SplInt))
                                [] [SplReturnStmt literalOne]])
        it "Does parse a function with two types and the same number of arguments" $
            parseSpl "fun (a, b) :: a b -> Int { return 1; }" `shouldParse`
                (Spl [SplDeclFun "fun" ["a", "b"] [SplTypePlaceholder "a", SplTypePlaceholder "b"]
                                (SplRetType (SplType SplInt))
                                [] [SplReturnStmt literalOne]])

    describe "stmt" $ do
        it "Parses return statements" $
            parseStmt "return 1;" `shouldParse`
                (SplReturnStmt literalOne)
        it "Parses return void statements" $
            parseStmt "return;" `shouldParse` SplReturnVoidStmt
        it "Parses simple assignment statements" $
            parseStmt "a = 1;" `shouldParse`
                (SplAssignmentStmt "a" SplFieldNone literalOne)
        it "Parses field assignment statements" $
            parseStmt "a.hd = 1;" `shouldParse`
                (SplAssignmentStmt "a" (SplFieldHd (SplFieldNone)) literalOne)
        it "Parses nested fiels assignment statements" $
            parseStmt "a.fst.hd = 1;" `shouldParse`
                (SplAssignmentStmt "a" (SplFieldFst $ SplFieldHd $ SplFieldNone) literalOne)
        it "parses updates of variables" $
            parseStmt "a = a + 1;" `shouldParse`
                (SplAssignmentStmt "a" SplFieldNone (SplBinaryExpr SplOperatorAdd (SplIdentifierExpr "a" SplFieldNone) literalOne))
        it "Parses empty if statements" $
            parseStmt "if (True) { }" `shouldParse` (SplIfStmt literalTrue [] [])
        it "Parses empty if statements with else" $
            parseStmt "if (True) { } else { }" `shouldParse` (SplIfStmt literalTrue [] [])
        it "Parses non-empty if statements" $
            parseStmt "if (True) { return 1; }" `shouldParse`
                (SplIfStmt literalTrue [SplReturnStmt literalOne] [])
        it "Parses non-empty if statements with empty else" $
            parseStmt "if (True) { return 1; } else { }" `shouldParse`
                (SplIfStmt literalTrue [SplReturnStmt literalOne] [])
        it "Parses non-empty if statements with non-empty else" $
            parseStmt "if (True) { return 1; } else { return 1; }" `shouldParse`
                (SplIfStmt literalTrue [SplReturnStmt literalOne] [SplReturnStmt literalOne])
        it "Parses non-empty else statements with empty else leaving nothing" $
            runParser' stmt (initialState "if (True) { return 1; } else { }") `succeedsLeaving` ""
        it "Should not parse else without if body" $
            parseStmt `shouldFailOn` "if (True) else { }"
        it "Should not parse if without {}" $
            parseStmt `shouldFailOn` "if (True) return 1;"
        it "Should not parse if without ()" $
            parseStmt `shouldFailOn` "if True {return 1;}"
        it "Should parse empty while statements" $
            parseStmt "while (True) { }" `shouldParse` (SplWhileStmt literalTrue [])
        it "Should parse non-empty while statements" $
            parseStmt "while (True) { return 1; }" `shouldParse`
                (SplWhileStmt literalTrue [SplReturnStmt literalOne])
        it "should parse a return of a tuple" $
            parseStmt "return (1, 1);" `shouldParse` (SplReturnStmt (SplTupleExpr literalOne literalOne))
        it "Should parse function calls" $
            parseStmt "fun ();" `shouldParse` (SplFuncCallStmt "fun" [])
        it "Should parse function calls with args" $
            parseStmt "fun (a);" `shouldParse`
                (SplFuncCallStmt "fun" [SplIdentifierExpr "a" SplFieldNone])
    where
        literalOne = SplIntLiteralExpr 1
        literalTrue = SplBooleanLiteralExpr True
        -- special runParser for succeedsLeaving
        parseSc x = runParser' sc (initialState x)
        parseIdentifier = parse identifier ""
        parseInt = parse int ""
        parseBool = parse bool ""
        parseChar = parse character ""
        parseBasicType = parse basicType ""
        parseType' = parse parseType ""
        parseSpl = parse spl ""
        parseStmt = parse stmt ""
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
