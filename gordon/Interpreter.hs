module Interpreter where
import Parser -- This is the Parser.hs file


-- Semantic domains for the language
data Value = Numeric Integer
  | Boolean Bool
  | ERROR
  deriving (Eq, Show)

data MemVal = Stored Value
  | Unbound
  deriving Show

-- Representation of the memory
type Memory = Ide -> MemVal -- Memory stores a value for an identifier
type Input = [Value]
type Output = [Value]
type State = (Memory, Input, Output) -- Triple of Memory, Input and Output


-- Display the memory
-- Works only for the identifiers x, y, z
display :: Memory -> String
display m = "x = " ++ show (m "x") ++ ", y = " ++ show (m "y") ++ ", z = " ++ show ( m "z") ++ " "

-- Semantic function definitions for Exp and Cmd
data ExpVal = OK Value State | Error
data CmdVal = OKc State | Errorc
exp_semantics :: Exp -> State -> ExpVal
cmd_semantics :: Cmd -> State -> CmdVal

-- Semantic function declarations for Expressions
exp_semantics Zero s = OK (Numeric 0) s
exp_semantics One s = OK (Numeric 1) s
exp_semantics Two s = OK (Numeric 2) s
exp_semantics Three s = OK (Numeric 3) s
exp_semantics TT s = OK (Boolean True) s
exp_semantics FF s = OK (Boolean False) s


-- Read the first value from the input
exp_semantics Read (m, [], o) =
  -- error ("Input required") -- If the input is empty, throw an error
  error (display m ++ "Input: " ++ "[] " ++ "Output: " ++ show o)
  
exp_semantics Read (m, i:is, o) = OK i (m, is, o) -- Read the first value

-- Check if the identifier is stored in the memory
exp_semantics (I ide) (m, i, o) = case m ide of
  Stored v -> OK v (m, i, o)
  -- Unbound -> Error
  Unbound   -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o ++ "\n Unbound identifier " ++ ide)

-- 'Not' expression can only be applied to boolean values
exp_semantics (Not exp) s = case (exp_semantics exp s) of
  OK (Boolean v) s -> OK (Boolean (not v)) s
  OK (Numeric _) s -> error ("\'not\' can only be applied to boolean values")
  _ -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
             where (m, i, o) = s

-- Check if the two expressions are equal
-- If they are equal, return OK, else Error
exp_semantics (Equal exp1 exp2) s =
  case (exp_semantics exp1 s, exp_semantics exp2 s) of
    (OK v1 s1, OK v2 s2) -> OK (Boolean (v1 == v2)) s
    _ -> error ("Both expressions must be of the same type")

-- Add two expressions, if both are numeric return the sum else return Error
exp_semantics (Plus exp1 exp2) s =
  case (exp_semantics exp1 s, exp_semantics exp2 s) of
    (OK (Numeric n1) s1, OK (Numeric n2) s2) -> OK (Numeric (n1 + n2)) s
    _ -> error ("Both expressions should be numeric")

-- Semantic function declarations for Commands

-- Helper functions to interact with the memory
-- Update the memory with the new value
update :: Memory -> Ide -> Value -> Memory
update memory ide val = \x -> if x == ide then Stored val else memory x

-- Search for the value of the identifier in the memory
search :: Memory -> Ide -> Value
search memory ide = case memory ide of
  Stored v -> v
  Unbound -> error ("Unbound identifier " ++ ide)

-- Initialize every new identifier with Unbound
emptymem ide = Unbound

-- Semantic function for Assign command
cmd_semantics (Assign ide exp) s =
  case (exp_semantics exp s) of
    OK v1 (m1, i1, o1) -> OKc (update m1 ide v1, i1, o1) -- Update the memory
    Error -> Errorc

-- Semantic function for Output command
cmd_semantics (Output exp) s =
  case (exp_semantics exp s) of
    OK v1 (m1, i1, o1) -> OKc (m1, i1, o1 ++ [v1])
    Error -> Errorc

-- Semantic function for IfThenElse command
cmd_semantics (IfThenElse exp cmd1 cmd2) s =
  case (exp_semantics exp s) of
    OK (Boolean True) s1 -> cmd_semantics cmd1 s1
    OK (Boolean False) s1 -> cmd_semantics cmd2 s1
    _ -> Errorc

-- Semantic function for WhileDo command
cmd_semantics (WhileDo exp cmd) s =
  case (exp_semantics exp s) of
    OK (Boolean True) s1 -> case (cmd_semantics cmd s1) of
      OKc s2 -> cmd_semantics (WhileDo exp cmd) s2
      Errorc -> Errorc
    OK (Boolean False) s1 -> OKc s1
    _ -> Errorc

-- Semantic function for Seq command
cmd_semantics (Seq cmd1 cmd2) s =
  case (cmd_semantics cmd1 s) of
    OKc s1 -> cmd_semantics cmd2 s1
    Errorc -> Errorc

-- Run the program with the given input
run program input =
  case (cmd_semantics parsed_program (emptymem, input, [])) of
    OKc (m, i, o) -> o
    Errorc -> [ERROR]
  where
    parsed_program = cparse program


-- Test cases
-- Test 1: run "output 1" []
-- Expected output: [Numeric 1]

-- Test 2: run "output 1" [Numeric 2]
-- Expected output: [Numeric 1]

-- Test 3: run "output 0; output 1" [Numeric 2]
-- Expected output: [Numeric 0, Numeric 1]

-- Test 4: run "if 1=1 then output 1 else output 0" []
-- Expected output: [Numeric 1]

-- Test 5: run "if 1=0 then output 1 else output 0" []
-- Expected output: [Numeric 0]

-- Test 6: run "while 1=1 do output 0" []
-- Expected output: Infinite loop, so the program will not terminate

-- Test 7: run "while 1=0 do output 0" []
-- Expected output: []

-- Test 8: run "x:=1; output x" []
-- Expected output: [Numeric 1]

-- Test 9: run "x:=0; y:=1; output x; output y" []
-- Expected output: [Numeric 0, Numeric 1]

-- Test 10: run "x:=0; y:=1; if x=0 then output y else output x" []
-- Expected output: [Numeric 1]
