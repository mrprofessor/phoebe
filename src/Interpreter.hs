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

-- Custome Ord class for data Value
instance Ord Value where
  compare (Numeric x) (Numeric y) = compare x y
  compare (Boolean x) (Boolean y) = compare x y
  compare ERROR ERROR = EQ
  compare ERROR _ = Error "Error is incomparable" 
  compare _ ERROR = Error "Error is incomparable" 
  compare (Numeric _) (Boolean _) = Error "Cannot compare Numeric with Boolean"
  compare (Boolean _) (Numeric _) = Error "Cannot compare Boolean with Numeric"
  
-- Representation of the memory
type Memory = Map Ide MemVal
type Input = [Value]
type Output = [Value]
type State = (Memory, Input, Output)

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
printState (m, i, o) = display m ++ "Input: " ++ show i ++ "Output: " ++ show o

-- Semantic function definitions for Exp and Cmd
data ExpVal = OK Value State | Error String
data CmdVal = OKc State | Errorc String
exp_semantics :: Exp -> State -> ExpVal
cmd_semantics :: Cmd -> State -> CmdVal

  -- Semantic function declarations for Expressions
exp_semantics (Number n) s = OK (Numeric n) s
exp_semantics (Bool b) s = OK (Boolean b) s

-- Read the first value from the input
-- If the input is empty, throw an error
exp_semantics Read (m, [], o) = Error "Input Required"
exp_semantics Read (m, i:is, o) = OK i (m, is, o) -- Read the first value

-- Check if the identifier is stored in the memory
exp_semantics (I ide) (m, i, o) =
  case (search m ide) of
    Stored v -> OK v (m, i, o)
    Unbound -> Error $ "Identifier " ++ ide ++ " not found in memory"

-- 'Not' expression can only be applied to boolean values
exp_semantics (Not exp) s = case (exp_semantics exp s) of
  OK (Boolean v) s -> OK (Boolean (not v)) s
  OK (Numeric _) s -> Error "\'not\' can only be applied to boolean values"
  _ -> Error "Invalid operand for 'not'" 

-- Check if the two expressions are equal
-- If they are equal, return OK, else Error
exp_semantics (Equal exp1 exp2) s =
  case (exp_semantics exp1 s, exp_semantics exp2 s) of
    (OK v1 s1, OK v2 s2) -> OK (Boolean (v1 == v2)) s
    _ -> Error "Equality comparison requires both operands to be of the same type"

-- Check if the first expression is greater than the second
exp_semantics (Greater exp1 exp2) s =
  case (exp_semantics exp1 s, exp_semantics exp2 s) of
    (OK v1 s1, OK v2 s2) -> OK (Boolean (v1 > v2)) s
    _ -> Error "Greater-than comparison requires both operands to be numeric"

-- Check if the first expression is lesser than the second
exp_semantics (Lesser exp1 exp2) s =
  case (exp_semantics exp1 s, exp_semantics exp2 s) of
    (OK v1 s1, OK v2 s2) -> OK (Boolean (v1 < v2)) s
    _ -> Error "Less-than comparison requires both operands to be numeric"

-- Add two expressions, if both are numeric return the sum else return Error
exp_semantics (Plus exp1 exp2) s =
  case (exp_semantics exp1 s, exp_semantics exp2 s) of
    (OK (Numeric n1) s1, OK (Numeric n2) s2) -> OK (Numeric (n1 + n2)) s
    _ -> Error "Addition requires both operands to be numeric"

-- Subtract two expressions
exp_semantics (Minus exp1 exp2) s =
  case (exp_semantics exp1 s, exp_semantics exp2 s) of
    (OK (Numeric n1) s1, OK (Numeric n2) s2) -> OK (Numeric (n1 - n2)) s
    _ -> Error "Substraction requires both operands to be numeric"

-- Semantic function declarations for Commands

-- Semantic function for Skip command (It does nothing)
cmd_semantics Skip s = OKc s

-- Semantic function for Assign command
cmd_semantics (Assign ide exp) s =
  case (exp_semantics exp s) of
    OK v1 (m1, i1, o1) -> OKc (update m1 ide v1, i1, o1) -- Update the memory
    Error msg -> Errorc $ "Assignment failed: " ++ msg

-- Semantic function for Output command
cmd_semantics (Output exp) s =
  case (exp_semantics exp s) of
    OK v1 (m1, i1, o1) -> OKc (m1, i1, o1 ++ [v1])
    Error msg -> Errorc $ "Output failed: " ++ msg

-- Semantic function for IfThenElse command
cmd_semantics (IfThenElse exp cmd1 cmd2) s =
  case (exp_semantics exp s) of
    OK (Boolean True) s1 -> cmd_semantics cmd1 s1
    OK (Boolean False) s1 -> cmd_semantics cmd2 s1
    _ -> Errorc "If-then-else requires a boolean condition"

-- Semantic function for WhileDo command
cmd_semantics (WhileDo exp cmd) s =
  case (exp_semantics exp s) of
    OK (Boolean True) s1 ->
      case (cmd_semantics cmd s1) of
        OKc s2 -> cmd_semantics (WhileDo exp cmd) s2
        Errorc msg -> Errorc msg
    OK (Boolean False) s1 -> OKc s1
    _ -> Errorc "While-do requires a boolean condition"

-- Semantic function for Seq command
cmd_semantics (Seq cmd1 cmd2) s =
  case (cmd_semantics cmd1 s) of
    OKc s1 -> cmd_semantics cmd2 s1
    Errorc msg -> Errorc msg

-- Run the program with the given input
run :: String -> [Value] -> [Value]
run program input = 
  case cparse program of
    Just parsed_program -> 
      case cmd_semantics parsed_program (emptymem, input, []) of
        OKc (_, _, o) -> o
        Errorc msg -> [ERROR]
    Nothing -> [ERROR]  -- Return ERROR if parsing fails
