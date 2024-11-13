-- Tests.hs
import Test.Hspec
import Parser
import Interpreter

-- Define test cases for expressions using eparse
testParseExpressions :: Spec
testParseExpressions = describe "Expression Parser Tests" $ do
  it "parses a number" $ do
    eparse "1"
      `shouldBe` Just (Number 1)

  it "parses a Boolean True value" $ do
    eparse "true"
      `shouldBe` Just (Bool True)
    
  it "parses a Boolean False value" $ do
    eparse "false"
      `shouldBe` Just (Bool False)

  it "parses a read value" $ do
    eparse "read"
      `shouldBe` Just Read

  it "parses an identifier" $ do
    eparse "x"
      `shouldBe` Just (I "x")

  it "parses a Not expression" $ do
    eparse "not true"
      `shouldBe` Just (Not (Bool True))

  it "parses an equality check" $ do
    eparse "1 = 1"
      `shouldBe` Just (Equal (Number 1) (Number 1))

  it "parses a greater than check" $ do
    eparse "1 > 1"
      `shouldBe` Just (Greater (Number 1) (Number 1))

  it "parses a lesser than check" $ do
    eparse "1 < 1"
      `shouldBe` Just (Lesser (Number 1) (Number 1))

  it "parses an addition" $ do
    eparse "1 + 1"
      `shouldBe` Just (Plus (Number 1) (Number 1))

  it "parses a subtraction" $ do
    eparse "1 - 1"
      `shouldBe` Just (Minus (Number 1) (Number 1))

  it "parses an expression with multiple operators" $ do
    eparse "5 + 3 - 2"
      `shouldBe` Just (Plus (Number 5) (Minus (Number 3) (Number 2)))

  it "parses a Not expression with an equality check" $ do
    eparse "not 1 = 1"
      `shouldBe` Just (Not (Equal (Number 1) (Number 1)))

testParseCommands :: Spec
testParseCommands = describe "Commands Parser Tests" $ do
  it "parses an output command" $ do
    cparse "output 10"
      `shouldBe` Just (Output (Number 10))

  it "parses an Assignment command" $ do
    cparse "x := 10"
      `shouldBe` Just (Assign "x" (Number 10))

  it "parses a Skip command" $ do
    cparse "skip"
      `shouldBe` Just Skip

  it "parses an IfThenElse command with a Skip command" $ do
    cparse "if true then skip else skip"
      `shouldBe` Just (IfThenElse (Bool True) Skip Skip)

  it "parses an IfThenElse command" $ do
    cparse "if true then output 10 else output 20"
      `shouldBe` Just (IfThenElse (Bool True)
                       (Output (Number 10))
                       (Output (Number 20)))

  it "parses a sequence of commands with an if-then-else" $ do
    cparse "if true then {x:=10; output x;} else {x:=20; output x;}"
      `shouldBe` Just (IfThenElse (Bool True)
                       (Seq (Assign "x" (Number 10)) (Output (I "x")))
                       (Seq (Assign "x" (Number 20)) (Output (I "x"))))

  it "parses a While command" $ do
    cparse "while true do output 10"
      `shouldBe` Just (WhileDo (Bool True) (Output (Number 10)))

  it "parses a sequence of commands with a while loop" $ do
    cparse "x:=10; while true do {x:=20; output x;}"
      `shouldBe` Just (Seq (Assign "x" (Number 10))
                     (WhileDo (Bool True)
                      (Seq (Assign "x" (Number 20)) (Output (I "x")))))

  it "parses a sequence of commands" $ do
    cparse "output 10; output 20"
      `shouldBe` Just (Seq (Output (Number 10)) (Output (Number 20)))

  it "parses a sequence of commands with ; at the end" $ do
    cparse "output 10; output 20;"
      `shouldBe` Just (Seq (Output (Number 10)) (Output (Number 20)))

  it "parses a sequence of commands with an assignment" $ do
    cparse "x := 10; output x"
      `shouldBe` Just (Seq (Assign "x" (Number 10)) (Output (I "x")))

  it "parses the test case from Gordon's example" $ do
      cparse "sum:=0; x:=read; while not (x=1) do sum:=sum+x; x:=read; output sum"
      `shouldBe` Just (Seq (Assign "sum" (Number 0))
                     (Seq (Assign "x" Read)
                      (Seq (WhileDo (Not (Equal (I "x") (Number 1)))
                            (Assign "sum" (Plus (I "sum") (I "x"))))
                        (Seq (Assign "x" Read) (Output (I "sum"))))))


testInterpreter :: Spec
testInterpreter = describe "Interpreter Tests" $ do
  it "interprets an output command" $ do
    run "output 10" []
      `shouldBe` [Numeric 10]

  it "interprets an assignment command with output" $ do
    run "x := 10; output x" []
      `shouldBe` [Numeric 10]

  it "interprets an assignment command without output" $ do
    run "x := 10" []
      `shouldBe` []

  it "interprets output and updates the value" $ do
    run "x := 10; output x" [Numeric 20]
      `shouldBe` [Numeric 10]

  it "interprets a read command" $ do
    run "x := read; output x" [Numeric 10]
      `shouldBe` [Numeric 10]

  it "interprets an if-then-else command" $ do
    run "x:=1; if x=1 then output 10 else output 20" []
      `shouldBe` [Numeric 10]

  it "interprets a while command" $ do
    run "count := 0; while not count=5 do count := count + 1; output count" []
      `shouldBe` [Numeric 5]

  it "interprets a sequence of commands" $ do
    run "output 10; output 20" []
      `shouldBe` [Numeric 10, Numeric 20]

  it "interprets a truty expression" $ do  
    run "output 10=10" []
      `shouldBe` [Boolean True]

  it "interprets a falsy expression" $ do
    run "output 10>10" []
      `shouldBe` [Boolean False]

  it "interprets a sequence of commands with an assignment" $ do
    run "x := 10; output x" []
      `shouldBe` [Numeric 10]

  it "interprets the test case from Gordon's example" $ do
    run "sum:=0; x:=read; while not (x=1) do {sum:=sum+x; x:=read}; output sum"
      [Numeric 7, Numeric 5, Numeric 3, Numeric 1]
      `shouldBe` [Numeric 15]

  it "interprets a if-then-else command within a while loop" $ do
    run "halt := false;        \
        \sum := 0;             \
        \while not halt do     \
        \  if sum = 5 then     \
        \    halt := true      \
        \  else                \
        \    {                 \
        \      sum := sum + 1; \
        \      output sum;     \
        \    }" []
      `shouldBe` [Numeric 1,Numeric 2,Numeric 3,Numeric 4,Numeric 5]


-- Run Tests
main :: IO ()
main = hspec $ do
  testParseExpressions
  testParseCommands
  testInterpreter
  -- TODO: Test the semantics of expressions and commands
