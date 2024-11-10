-- Tests.hs
import Test.HUnit
import Parser


-- Define test cases for expressions using eparse
testExpressions :: Test
testExpressions = TestList
  [ TestCase (assertEqual "Parse simple addition" 
      (Plus (Number 1) (Number 1)) 
      (eparse "1 + 1"))

  , TestCase (assertEqual "Parse simple Subtraction" 
      (Minus (Number 1) (Number 1)) 
      (eparse "1 - 1"))

  , TestCase (assertEqual "Parse equality check" 
      (Equal (Number 1) (Number 1)) 
      (eparse "1 = 1"))
  ]

-- Run Tests
main :: IO Counts
main = runTestTT testExpressions

