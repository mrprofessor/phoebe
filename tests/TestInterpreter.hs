module TestInterpreter where

import Test.Hspec
import Interpreter

assertEvaluatesTo :: String -> [Value] -> [Value] -> Expectation
assertEvaluatesTo program input expectedOutput = do
  let result = run program input
  flattenResults result `shouldBe` expectedOutput

assertErrors :: String -> [Value] -> String -> Expectation
assertErrors program input expectedError =
    case run program input of
    ErrorState msg -> msg `shouldBe` expectedError
    other -> expectationFailure $ "Expected an error, but got: " ++ show other

spec :: Spec
spec = do
    describe "Basic Operations" $ do
        it "evaluates numeric constants" $
            assertEvaluatesTo
            "program begin const x = 42; output x end"
            []
            [Numeric 42]

        it "evaluates arithmetic operations" $
            assertEvaluatesTo
            "program output 3 + 4 * 2"  -- program C
            []
            [Numeric 11]

        it "evaluates boolean operations" $
            assertEvaluatesTo
            "program output true == false"
            []
            [Boolean False]

        it "evaluates string constants" $
            assertEvaluatesTo
            "program begin const msg = \"hello\"; output msg end"
            []
            [Str "hello"]

    describe "Variables and Assignment" $ do
        it "handles variable declaration and assignment" $
            assertEvaluatesTo
            "program begin var x = 10; x := 20; output x end"
            []
            [Numeric 20]

        it "maintains variable scope" $
                assertEvaluatesTo
                "program begin var x = 1; begin var x = 2; output x end; output x end"
                []
                [Numeric 2, Numeric 1]

        it "prevents reassignment to constants" $
            assertErrors
            "program begin const x = 10; x := 20 end"
            []
            "Invalid location on the left-hand side"

    describe "Control Flow" $ do
        it "evaluates if-then-else correctly" $
            assertEvaluatesTo
            "program begin var x = 10; if x > 5 then output 1 else output 0 end"
            []
            [Numeric 1]

        it "executes while loops" $
            assertEvaluatesTo
            "program begin var x = 3; while x > 0 do { output x; x := x - 1 } end"
            []
            [Numeric 3, Numeric 2, Numeric 1]

    describe "Arrays" $ do
        it "handles array declaration and access" $
            assertEvaluatesTo
            "program begin array nums[1:3]; nums[1] := 42; output nums[1] end"
            []
            [Numeric 42]

        it "enforces array bounds" $
            assertErrors
            "program begin array nums[1:3]; nums[0] := 42 end"
            []
            "Array index 0 out of bounds [1..3]"

        it "formats array output" $
            assertEvaluatesTo
            "program begin array nums[1:3]; nums[1] := 10; nums[2] := 20; output nums end"
            []
            [Str "{1: Numeric 10, 2: Numeric 20, 3: Error \"Unassigned\"}"]

    describe "Records" $ do
        it "handles record declaration and field access" $
            assertEvaluatesTo
            "program begin record point(x, y); point.x := 42; output point.x end"
            []
            [Numeric 42]

        it "prevents access to undefined fields" $
            assertErrors
            "program begin record point(x, y); point.z := 42 end"
            []
            "Unexpected environment value for: z"

        it "formats record output" $
            assertEvaluatesTo
            "program begin record point(x, y); point.x := 1; point.y := 2; output point end"
            []
            [Str "{x: Numeric 1, y: Numeric 2}"]

    describe "Functions" $ do
        it "evaluates simple functions" $
            assertEvaluatesTo
            "program begin fun add(x,y) -> x + y; output add!(2,3) end"
            []
            [Numeric 5]

        it "handles recursive functions" $
            assertEvaluatesTo
            "program begin rec fun fact(n) -> if n == 0 then 1 else n * fact!(n-1); output fact!(5) end"
            []
            [Numeric 120]

    describe "Procedures" $ do
        it "executes procedures with pass-by-value" $
            assertEvaluatesTo
            "program begin var x = 1; proc inc(n) -> n := n + 1; inc(x); output x end"
            []
            [Numeric 1]  -- x shouldn't change because n is passed by value

        it "executes procedures with pass-by-reference" $
            assertEvaluatesTo
            "program begin var x = 1; proc inc(var n) -> n := n + 1; inc(x); output x end"
            []
            [Numeric 2]  -- x should change because n is passed by reference

    describe "Error Handling" $ do
        it "handles division by zero" $
            assertErrors
            "program output 1/0"
            []
            "Division by zero"

        it "handles undefined variables" $
            assertErrors
            "program output x"
            []
            "Unexpected environment value for: x"

        it "handles type mismatches" $
            assertErrors
            "program output true + 1"
            []
            "Type mismatch : RValue Boolean True + RValue Numeric 1"

    describe "Input/Output" $ do
        it "reads input correctly" $
            assertEvaluatesTo
            "program output read"
            [Numeric 42]
            [Numeric 42]

        it "handles multiple reads" $
            assertEvaluatesTo
            "program { output read; output read }"
            [Numeric 1, Numeric 2]
            [Numeric 1, Numeric 2]

        it "handles empty input" $
            assertErrors
            "program output read"
            []
            "No Input Provided"
