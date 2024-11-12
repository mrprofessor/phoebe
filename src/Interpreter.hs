module Interpreter where
import Data.Map (Map)
import qualified Data.Map as Map

import Parser  -- Import the parser module

-- Semantic domains for the language
data Value = Numeric Integer
  | Boolean Bool
  | ERROR
  deriving (Eq, Show)

data MemVal = Stored Value
  | Unbound
  deriving Show

-- Representation of the memory
type Memory = Map Ide MemVal
type Input = [Value]
type Output = [Value]
type State = (Memory, Input, Output) -- Triple of Memory, Input and Output

-- Initial Empty Memory
emptymem :: Memory
emptymem = Map.empty

-- Display the memory
display :: Memory -> String
display m = "Memory:\n" ++ show (Map.toList m)

-- Helper functions to interact with the memory
-- Update the memory with a new value
update :: Memory -> Ide -> Value -> Memory
update memory ide val = Map.insert ide (Stored val) memory

-- Search for the value of the identifier in the memory
search :: Memory -> Ide -> MemVal
search memory ide = case Map.lookup ide memory of
  Just (Stored v) -> Stored v
  _ -> Unbound

  -- Print the State
printState :: State -> String
printState (m, i, o) = display m ++ "Input: " ++ show i ++ "\nOutput: " ++ show o

-- Semantic function definitions for Exp and Cmd
data ExpVal = OK Value State | Error
data CmdVal = OKc State | Errorc
exp_semantics :: Exp -> State -> ExpVal
cmd_semantics :: Cmd -> State -> CmdVal

  -- Semantic function declarations for Expressions
exp_semantics (Number n) s = OK (Numeric n) s
exp_semantics (Bool b) s = OK (Boolean b) s

-- Read the first value from the input
-- error ("Input required") -- If the input is empty, throw an error
exp_semantics Read (m, [], o) = error (printState (m, [], o))
exp_semantics Read (m, i:is, o) = OK i (m, is, o) -- Read the first value

-- Check if the identifier is stored in the memory
exp_semantics (I ide) s = case (search m ide) of
  Stored v -> OK v s
  Unbound -> error ("Identifier " ++ ide ++ " not found in memory\n" ++ printState s)
  where (m, _, _) = s

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

-- Subtract two expressions
exp_semantics (Minus exp1 exp2) s =
  case (exp_semantics exp1 s, exp_semantics exp2 s) of
    (OK (Numeric n1) s1, OK (Numeric n2) s2) -> OK (Numeric (n1 - n2)) s
    _ -> error ("Both expressions should be numeric")

-- Semantic function declarations for Commands

-- Semantic function for Skip command (It does nothing)
cmd_semantics Skip s = OKc s

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
