import Test.Hspec
import Interpreter

-- Equality Clauses for test
instance Eq ParsedResult where
  (ParseOk p1) == (ParseOk p2) = show p1 == show p2
  (ParseError e1) == (ParseError e2) = e1 == e2
  _ == _ = False

instance Eq Ans where
  Stop (_, _, _, _, output1) == Stop (_, _, _, _, output2) = output1 == output2
  ErrorState msg1 == ErrorState msg2 = msg1 == msg2
  _ == _ = False


data TestType = ParserTest | InterpreterTest

data TestCase = TestCase
  { description :: String
  , testType :: TestType
  , programStr :: String
  , input :: Maybe [Value]
  , expected :: Either ParsedResult Ans
  }

testCases :: [TestCase]
testCases =
  [
    TestCase
      "variable declaration with output (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    var x = 42;",
        "    output x;",
        "  end"
      ])
      Nothing
      (Left $ ParseOk $ Program 
        (BeginEnd 
          (Variable "x" (Number 42)) 
          (Output (Identifier "x")))),

    TestCase
      "constant declaration with output (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    const x = 10;",
        "    output x;",
        "  end"
      ])
      Nothing
      (Left $ ParseOk $ Program 
        (BeginEnd 
          (Constant "x" (Number 10))
          (Output (Identifier "x")))),

    TestCase
      "procedure declaration with call (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    proc print (x), output x;",
        "    print(10);",
        "  end"
      ])
      Nothing
      (Left $ ParseOk $ Program 
        (BeginEnd 
          (Procedure "print" ["x"] (Output (Identifier "x")))
          (CallProc "print" [Number 10]))),

    TestCase
      "function declaration with call (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    fun inc (x), x+1;",
        "    output inc!(10);",
        "  end"
      ])
      Nothing
      (Left $ ParseOk $ Program 
        (BeginEnd 
          (Function "inc" ["x"] 
            (BinOp "+" (Identifier "x") (Number 1)))
          (Output (CallFun "inc" [Number 10])))),

    TestCase
      "factorial function with call (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    fun fact(n),",
        "    if n == 0 then 1",
        "    else n * fact!(n-1);",
        "    output fact!(10);",
        "  end"
      ])
      Nothing
      (Left $ ParseOk $ Program 
        (BeginEnd 
          (Function "fact" ["n"] 
            (IfExp 
              (BinOp "==" (Identifier "n") (Number 0))
              (Number 1)
              (BinOp "*" 
                (Identifier "n") 
                (CallFun "fact" [BinOp "-" (Identifier "n") (Number 1)]))))
          (Output (CallFun "fact" [Number 10])))),

    TestCase
      "missing commands (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    var x = 10;",
        "  end"
      ])
      Nothing
      (Left $ ParseError "Invalid input"),

    TestCase
      "missing declarations (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    output 100;",
        "  end"
      ])
      Nothing
      (Left $ ParseError "Invalid input"),

    TestCase
      "missing variable declaration keyword (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    x = 42;",
        "    output x;",
        "  end"
      ])
      Nothing
      (Left $ ParseError "Invalid input"),

    TestCase
      "missing semicolon (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    var x = 42",  -- no semicolon
        "    var y = 10;",
        "    output x;",
        "  end"
      ])
      Nothing
      (Left $ ParseError "Invalid input"),

    TestCase
      "incorrect procedure syntax (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    proc badProc x,",  -- missing parentheses
        "    output x;",
        "    output 42;",
        "  end"
      ])
      Nothing
      (Left $ ParseError "Invalid input"),

    -- Interpreter Tests
    TestCase
      "variable declaration and output (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    var x = 42;",
        "    output x;",
        "  end"
      ])
      (Just [])
      (Right $ Stop (defaultEnv, defaultStore, 1, [], [Numeric 42])),

    TestCase
      "constant declaration and output (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    const x = 100;",
        "    output x;",
        "  end"
      ])
      (Just [])
      (Right $ Stop (defaultEnv, defaultStore, 0, [], [Numeric 100])),

    TestCase
      "Function declaration and assignment (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    fun add(x,y), x+y;",
        "    var x = 1;",
        "    var y = 2;",
        "    var z = add!(1,2);",
        "    output z;",
        "  end"
      ])
    (Just [])
      (Right $ Stop (defaultEnv, defaultStore, 0, [], [Numeric 3])),

    TestCase
      "Function declaration and dynamic binding (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    fun add(x,y), x+y;",
        "    var x = 1;",
        "    var y = 2;",
        "    var z = add!(x,y);",
        "    output z;",
        "  end"
      ])
      (Just [])
      (Right $ Stop (defaultEnv, defaultStore, 0, [], [Numeric 3])),

    -- Error cases
    TestCase
      "accessing undefined variable (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    var y = 10;",
        "    output x;",
        "  end"
      ])
      (Just [])
      (Right $ ErrorState "Undefined identifier: x"),

    TestCase
      "division by zero (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    var x = 42;",
        "    output x / 0;",
        "  end"
      ])
      (Just [])
      (Right $ ErrorState "Division by zero")
  ]

testSuite :: Spec
testSuite = describe "Phoebe Language Tests" $ do
  mapM_ runTest testCases
  where
    runTest TestCase{description, testType, programStr, input, expected} =
      it description $ case testType of
        ParserTest -> sparse programStr `shouldBe`
                      either id (error "Expected ParsedResult") expected
        InterpreterTest -> case input of
          Just inp -> run programStr inp `shouldBe`
                      either (error "Expected Ans") id expected
          Nothing -> error "Interpreter test requires input"

runTests :: IO ()
runTests = hspec testSuite
