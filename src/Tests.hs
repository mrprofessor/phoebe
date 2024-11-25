-- Tests.hs
import Test.Hspec
import AST

instance Eq ParsedResult where
  (ParseOk p1) == (ParseOk p2) = show p1 == show p2
  (ParseError e1) == (ParseError e2) = e1 == e2
  _ == _ = False

data TestCase = TestCase 
  {
    description :: String,
    input :: String,
    expected :: ParsedResult
  }

testCases :: [TestCase]
testCases = [
    -- Success cases
    TestCase 
      "variable declaration with output"
      (unlines [
        "var x = 42;",
        "output x"
      ])
      (ParseOk $ Program 
        (Variable "x" (Number 42))
        (Output (VarRef "x"))),

    TestCase
      "constant declaration with output"
      (unlines [
        "const x = 10;",
        "output x"
      ])
      (ParseOk $ Program
        (Constant "x" (Number 10))
        (Output (VarRef "x"))),

    TestCase
      "procedure declaration with call"
      (unlines [
        "proc print (x), output x;",
        "print(10)"
      ])
      (ParseOk $ Program
        (Procedure "print" ["x"] 
          (Output (VarRef "x")))
        (CallProc "print" [Number 10])),

    TestCase
      "function declaration with call"
      (unlines [
        "fun inc (x), x+1;",
        "output inc!(10)"
      ])
      (ParseOk $ Program
        (Function "inc" ["x"] 
          (BinOp "+" (VarRef "x") (Number 1)))
        (Output (CallFun "inc" [Number 10]))),

    TestCase 
        "factorial function with call"
        (unlines [
            "fun fact(n),",
            "if n == 0 then 1",
            "else n * fact!(n-1);",
            "output fact!(10)"
        ])
        (ParseOk $ Program
            (Function "fact" ["n"] 
                (IfExp 
                    (BinOp "==" (VarRef "n") (Number 0))
                    (Number 1)
                    (BinOp "*" 
                        (VarRef "n") 
                        (CallFun "fact" [BinOp "-" (VarRef "n") (Number 1)])
                    )
                ))
            (Output (CallFun "fact" [Number 10]))
        ),

    -- Error cases
    TestCase
      "missing commands"
      (unlines [
        "var x = 10;",
        "var y = 20;"
      ])
      (ParseError "Invalid input"),

    TestCase
      "missing declarations"
      (unlines [
        "output 100;"
      ])
      (ParseError "Invalid input"),

    TestCase
      "missing variable declaration keyword"
      (unlines [
        "x = 42;",
        "output x"
      ])
      (ParseError "Invalid input"),

    TestCase
      "missing semicolon"
      (unlines [
        "var x = 42",  -- note: no semicolon here
        "var y = 10;",
        "output x"
      ])
      (ParseError "Invalid input"),

    TestCase
      "incorrect procedure syntax"
      (unlines [
        "proc badProc x,",  -- missing parentheses
        "  output x;",
        "  output 42"
      ])
      (ParseError "Invalid input")
  ]

testParser :: Spec
testParser = describe "Phoebe Language Parser" $ do
  describe "Success Cases" $
    mapM_ testSuccess $ filter isSuccess testCases
  
  describe "Error Cases" $
    mapM_ testError $ filter (not . isSuccess) testCases
  where
    testSuccess TestCase{description, input, expected} =
      it description $
        sparse input `shouldBe` expected
    
    testError TestCase{description, input, expected} =
      it description $
        sparse input `shouldBe` expected

isSuccess :: TestCase -> Bool
isSuccess TestCase{expected = ParseOk _} = True
isSuccess _ = False

main :: IO ()
main = hspec testParser
