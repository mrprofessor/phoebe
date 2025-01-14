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
      "Passes: variable declaration with output (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    var x = 42;",
        "    output x",
        "  end"
      ])
      Nothing
      (Left $ ParseOk $ Program 
        (BeginEnd 
          (Variable "x" (Number 42)) 
          (Output (Identifier "x")))),

    TestCase
      "Passes: constant declaration with output (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    const x = 10;",
        "    output x",
        "  end"
      ])
      Nothing
      (Left $ ParseOk $ Program 
        (BeginEnd 
          (Constant "x" (Number 10))
          (Output (Identifier "x")))),

    TestCase
      "Passes: procedure declaration with call (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    proc print (x), output x;",
        "    print(10)",
        "  end"
      ])
      Nothing
      (Left $ ParseOk $ Program 
        (BeginEnd 
          (Procedure "print" [ValueParam "x"] (Output (Identifier "x")))
          (CallProc "print" [Number 10]))),

    TestCase
    "Passes: function declaration with call (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    fun inc (x), x+1;",
        "    output inc!(10)",
        "  end"
      ])
      Nothing
      (Left $ ParseOk $ Program 
        (BeginEnd 
          (Function "inc" [ValueParam "x"] 
            (BinOp "+" (Identifier "x") (Number 1)))
          (Output (CallFun "inc" [Number 10])))),

    TestCase
      "Passes: IfElseThen Command (Parser)"
      ParserTest
      (unlines [
          "program",
          "  begin",
          "    var x = read;",
          "    if (x/2) == 1 then output true",
          "    else",
          "      begin",
          "        var x = x + 1;",
          "        output x",
          "      end",
          "    end"
      ])
      Nothing
      (Left $ ParseOk $ Program 
      (BeginEnd 
          (Variable "x" Read)
          (IfCom
          (BinOp "==" 
              (BinOp "/" (Identifier "x") (Number 2)) 
              (Number 1))
          (Output (Bool True))
          (BeginEnd
              (Variable "x" 
              (BinOp "+" (Identifier "x") (Number 1)))
              (Output (Identifier "x")))))),

    TestCase
      "Passes: factorial function with call (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    rec fun fact(n),",
        "    if n == 0 then 1",
        "    else n * fact!(n-1);",
        "    output fact!(10)",
        "  end"
      ])
      Nothing
      (Left $ ParseOk $ Program 
        (BeginEnd 
          (RecFunction "fact" [ValueParam "n"] 
            (IfExp 
              (BinOp "==" (Identifier "n") (Number 0))
              (Number 1)
              (BinOp "*" 
                (Identifier "n") 
                (CallFun "fact" [BinOp "-" (Identifier "n") (Number 1)]))))
          (Output (CallFun "fact" [Number 10])))),

    TestCase
      "Passes: missing commands in BeginEnd (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    var x = 10",
        "  end"
      ])
      Nothing
      (Left $ ParseError "Invalid input"),

    TestCase
      "Fails: missing declarations in BeginEnd (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    output 100",
        "  end"
      ])
      Nothing
      (Left $ ParseError "Invalid input"),

    TestCase
      "Fails: missing variable declaration keyword (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    x = 42;",
        "    output x",
        "  end"
      ])
      Nothing
      (Left $ ParseError "Invalid input"),

    TestCase
      "Fails: missing semicolon (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    var x = 42",  -- no semicolon
        "    var y = 10;",
        "    output x",
        "  end"
      ])
      Nothing
      (Left $ ParseError "Invalid input"),

    TestCase
      "Fails: incorrect procedure syntax (Parser)"
      ParserTest
      (unlines [
        "program",
        "  begin",
        "    proc badProc x,",  -- missing parentheses
        "    output x;",
        "    output 42",
        "  end"
      ])
      Nothing
      (Left $ ParseError "Invalid input"),

    -- Interpreter Tests
    TestCase
      "Passes: variable declaration and output (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    var x = 42;",
        "    output x",
        "  end"
      ])
      (Just [])
      (Right $ Stop (defaultEnv, defaultStore, 1, [], [Numeric 42])),

    TestCase
      "Passes: constant declaration and output (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    const x = 100;",
        "    output x",
        "  end"
      ])
      (Just [])
      (Right $ Stop (defaultEnv, defaultStore, 0, [], [Numeric 100])),

    TestCase
      "Passes: function declaration and assignment (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    fun add(x,y), x+y;",
        "    var x = 1;",
        "    var y = 2;",
        "    var z = add!(1,2);",
        "    output z",
        "  end"
      ])
    (Just [])
      (Right $ Stop (defaultEnv, defaultStore, 0, [], [Numeric 3])),

    TestCase
      "Passes: function declaration and dynamic binding (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    fun add(x,y), x+y;",
        "    var x = 1;",
        "    var y = 2;",
        "    var z = add!(x,y);",
        "    output z",
        "  end"
      ])
      (Just [])
      (Right $ Stop (defaultEnv, defaultStore, 0, [], [Numeric 3])),

    TestCase
      "Passes: factorial function with call (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    rec fun fact(n),",
        "    if n == 0 then 1",
        "    else n * fact!(n-1);",
        "    output fact!(5)",
        "  end"
      ])
      (Just [])
      (Right $ Stop (defaultEnv, defaultStore, 0, [], [Numeric 120])),

    TestCase
      "Passes: procedure with pass-by-value parameters (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    proc swap(a, b),",
        "      begin",
        "        var temp = a;",
        "        a := b;",
        "        b := temp",
        "      end;",
        "    var x = 5;",
        "    var y = 10;",
        "    swap(x, y);",
        "    output x;",
        "    output y",
        "  end"
      ])
      (Just [])
      (Right $ Stop (defaultEnv, defaultStore, 0, [], [Numeric 5, Numeric 10])),

    TestCase
      "Passes: procedure with pass-by-reference parameters (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    proc swap(var a, var b),",
        "      begin",
        "        var temp = a;",
        "        a := b;",
        "        b := temp",
        "      end;",
        "    var x = 5;",
        "    var y = 10;",
        "    swap(x, y);",
        "    output x;",
        "    output y",
        "  end"
      ])
      (Just [])
      (Right $ Stop (defaultEnv, defaultStore, 0, [], [Numeric 10, Numeric 5])),

    TestCase
      "Passes: trap block with escapeto (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    var x = 1;",
        "    trap {",
        "      output(x);",
        "      escapeto label1;",
        "      output(999)",
        "      }",
        "      label1: output(100)",
        "    end",
        "  end"
      ])
      (Just [])
      (Right $ Stop (defaultEnv, defaultStore, 0, [], [Numeric 1, Numeric 100])),

    TestCase
      "Passes: trap with conditional escapeto (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    var x = 5;",
        "    trap {",
        "      if x > 3 then",
        "        escapeto label1",
        "      else",
        "        escapeto label2",
        "      }",
        "      label1: output(10),",
        "      label2: output(20)",
        "    end",
        "  end"
      ])
      (Just [])
      (Right $ Stop (defaultEnv, defaultStore, 0, [], [Numeric 10])),

    -- Error cases
    TestCase
      "Fails: accessing undefined variable (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    var y = 10;",
        "    output x",
        "  end"
      ])
      (Just [])
      (Right $ ErrorState "Undefined identifier: x"),

    TestCase
    "Fails: division by zero (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    var x = 42;",
        "    output x / 0",
        "  end"
      ])
      (Just [])
      (Right $ ErrorState "Division by zero"),

    TestCase
      "Fails: pass-by-reference with non-identifier argument (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    proc swap(var a, var b),",
        "      begin",
        "        var temp = a;",
        "        a := b;",
        "        b := temp",
        "      end;",
        "    var x = 5;",
        "    var y = 10;",
        "    swap(x, 3);",
        "    output x;",
        "    output y",
        "  end"
      ])
      (Just [])
      (Right $ ErrorState "Pass by reference argument must be an identifier"),

    TestCase
      "Fails: pass-by-reference with constant argument (Interpreter)"
      InterpreterTest
      (unlines [
        "program",
        "  begin",
        "    proc swap(var a, var b),",
        "      begin",
        "        var temp = a;",
        "        a := b;",
        "        b := temp",
        "      end;",
        "    var x = 5;",
        "    const y = 10;",
        "    swap(x, y);",
        "    output x;",
        "    output y",
        "  end"
      ])
      (Just [])
      (Right $ ErrorState "Pass by reference requires a variable, got: y")
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
