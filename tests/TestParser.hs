module TestParser where

import Test.Hspec
import Parser

assertParses :: String -> Program -> Expectation
assertParses input expected = do
  let result = sparse input
  case result of
    ParseOk prog -> prog `shouldBe` expected
    ParseError msg -> expectationFailure $ "Failed to parse: " ++ msg

assertFails :: String -> String -> Expectation
assertFails input expectedError = do
  let result = sparse input
  case result of
    ParseOk prog -> expectationFailure $ "Expected parse error but got: " ++ show prog
    ParseError msg -> msg `shouldBe` expectedError

-- Actual test cases
spec :: Spec
spec = do
  describe "Basic Expressions" $ do
    it "parses numeric literals" $
      assertParses
        "program output 42"
        (Program (Output (Number 42)))

    it "parses boolean literals" $ do
      assertParses
        "program output true"
        (Program (Output (Bool True)))
      assertParses
        "program output false"
        (Program (Output (Bool False)))

    it "parses string literals" $
      assertParses
        "program output \"hello\""
        (Program (Output (String "hello")))

    it "parses identifiers" $
      assertParses
        "program output x"
        (Program (Output (Identifier "x")))

  describe "Arithmetic Operations" $ do
    it "parses addition" $
      assertParses
        "program output 1 + 2"
        (Program (Output (BinOp "+" (Number 1) (Number 2))))

    it "parses multiplication" $
      assertParses
        "program output 3 * 4"
        (Program (Output (BinOp "*" (Number 3) (Number 4))))

    it "parses complex expressions" $
      assertParses
        "program output 1 + 2 * 3"
        (Program (Output (BinOp "+" (Number 1) (BinOp "*" (Number 2) (Number 3)))))

  describe "Boolean Operations" $ do
    it "parses comparisons" $ do
      assertParses
        "program output x <= y"
        (Program (Output (BinOp "<=" (Identifier "x") (Identifier "y"))))
      assertParses
        "program output x >= y"
        (Program (Output (BinOp ">=" (Identifier "x") (Identifier "y"))))

    it "parses equality" $
      assertParses
        "program output x == y"
        (Program (Output (BinOp "==" (Identifier "x") (Identifier "y"))))

  describe "Variable Declarations" $ do
    it "parses variable declarations" $
      assertParses
        "program begin var x = 42; output x end"
        (Program (BeginEnd
          (Variable "x" (Number 42))
          (Output (Identifier "x"))))

    it "parses constant declarations" $
      assertParses
        "program begin const x = 42; output x end"
        (Program (BeginEnd
          (Constant "x" (Number 42))
          (Output (Identifier "x"))))

  describe "Control Flow" $ do
    it "parses if expressions" $
      assertParses
        "program if true then output 1 else output 0"
        (Program (IfCmd (Bool True)
                       (Output (Number 1))
                       (Output (Number 0))))

    it "parses while loops" $
      assertParses
        "program while x > 0 do output x"
        (Program (WhileDo
          (BinOp ">" (Identifier "x") (Number 0))
          (Output (Identifier "x"))))

  describe "Arrays" $ do
    it "parses array declarations" $
      assertParses
        "program begin array nums[1:5]; output nums[1] end"
        (Program (BeginEnd
          (Array "nums" (Number 1) (Number 5))
          (Output (ArrayAccess (Identifier "nums") (Number 1)))))

    it "parses array assignments" $
      assertParses
        "program begin array nums[1:5]; nums[1] := 42 end"
        (Program (BeginEnd
          (Array "nums" (Number 1) (Number 5))
          (Assign
            (ArrayAccess (Identifier "nums") (Number 1))
            (Number 42))))

  describe "Records" $ do
    it "parses record declarations" $
      assertParses
        "program begin record point(x, y); output point.x end"
        (Program (BeginEnd
          (Record "point" ["x", "y"])
          (Output (RecordAccess (Identifier "point") (Identifier "x")))))

    it "parses record assignments" $
      assertParses
        "program begin record point(x, y); point.x := 42 end"
        (Program (BeginEnd
          (Record "point" ["x", "y"])
          (Assign
            (RecordAccess (Identifier "point") (Identifier "x"))
            (Number 42))))

  describe "Functions and Procedures" $ do
    it "parses function declarations" $
      assertParses
        "program begin fun add(x,y) -> x + y; output add!(1,2) end"
        (Program (BeginEnd
          (Function "add" [ValueParam "x", ValueParam "y"]
            (BinOp "+" (Identifier "x") (Identifier "y")))
          (Output (CallFun "add" [Number 1, Number 2]))))

    it "parses recursive functions" $
      assertParses
        "program begin rec fun fact(n) -> if n == 0 then 1 else n * fact!(n-1); output fact!(5) end"
        (Program (BeginEnd
          (RecFunction "fact" [ValueParam "n"]
            (IfExp
              (BinOp "==" (Identifier "n") (Number 0))
              (Number 1)
              (BinOp "*"
                (Identifier "n")
                (CallFun "fact" [BinOp "-" (Identifier "n") (Number 1)]))))
          (Output (CallFun "fact" [Number 5]))))

    it "parses procedure declarations" $
      assertParses
        "program begin proc print(x) -> output x; print(42) end"
        (Program (BeginEnd
          (Procedure "print" [ValueParam "x"] (Output (Identifier "x")))
          (CallProc "print" [Number 42])))

  describe "Error Cases" $ do
    it "fails on missing semicolons" $
      assertFails
        "program begin var x = 42 var y = 43 end"
        "Invalid Syntax"

    it "fails on missing end keyword" $
      assertFails
        "program begin var x = 42;"
        "Invalid Syntax"

    it "fails on invalid begin-end block without declaration" $
        assertFails
        "program begin output 42 end"
        "Invalid Syntax"
