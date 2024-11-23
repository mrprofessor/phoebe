module Micro where
import Parsing

import Data.Map (Map)
import qualified Data.Map as Map

import System.IO (hFlush, stdout) 

type Ide = String

data Exp
  = Number Integer
  | Bool Bool
  | I Ide
  | Not Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Equal Exp Exp
  | Read
  | Skip
  | Output Exp
  | Assign Ide Exp
  | Seq Exp Exp
  | WhileDo Exp Exp
  | IfThenElse Exp Exp Exp
  deriving Show

data ParsedResult
  = ParseOk Exp
  | ParseError String
  deriving Show

exprSeq :: Parser Exp
exprSeq =
  do e1 <- expr
     (do symbol ";"
         rest <- exprSeq
         return (Seq e1 rest)
      +++
      do symbol ";"
         return (e1)
      +++
      return (e1))

expr :: Parser Exp
expr =
  do symbol "skip"
     return Skip
  +++
  do symbol "read"
     return Read
  +++
  do e1 <- term
     symbol "+"
     e2 <- expr
     return (Plus e1 e2)
  +++
  do e1 <- term
     symbol "-"
     e2 <- expr
     return (Minus e1 e2)
  +++
  do e1 <- term
     symbol "=="
     e2 <- expr
     return (Equal e1 e2)
  +++
  do symbol "output"
     e <- expr
     return (Output e)
  +++
  do id <- token identifier
     symbol ":="
     e <- expr
     return (Assign id e)
  +++
  do symbol "while"
     e1 <- expr
     symbol "do"
     e2 <- expr
     return (WhileDo e1 e2)
  +++
  do symbol "if"
     e1 <- expr
     symbol "then"
     e2 <- expr
     symbol "else"
     e3 <- expr
     return (IfThenElse e1 e2 e3)
  +++
     term

term :: Parser Exp
term =
  do symbol "not"
     e <- expr
     return (Not e)
  +++
     factor

factor :: Parser Exp
factor =
  do n <- nat
     return (Number (toInteger n))
  +++
  do symbol "true"
     return (Bool True)
  +++
  do symbol "false"
     return (Bool False)
  +++
  do id <- token identifier
     return (I id)
  +++
  do symbol "("
     e <- expr
     symbol ")"
     return e
  +++
  do symbol "{"
     c <- exprSeq
     symbol "}"
     return c

eparse :: String -> ParsedResult
eparse xs = case parse exprSeq xs of
              [(result, [])]   -> ParseOk result
              [(result, out_)] -> ParseError ("Unused input " ++ out_)
              []               -> ParseError "Invalid Input"


-------------------------------------------------------------------------------
-- Interpreter
-------------------------------------------------------------------------------

data Value
  = None
  | Numeric Integer
  | Boolean Bool
  | Error String
  deriving (Eq, Show)

data MemVal
  = Stored Value
  | Unbound
  deriving Show

-- Memory
type Memory = Map Ide MemVal
type Input = [Value]
type Output = [Value]
type State = (Memory, Input, Output)

-- Initialize an empty Memory
emptyMem :: Memory
emptyMem = Map.empty

-- Helper functions to interact with the memory
update :: Memory -> Ide -> Value -> Memory
update memory ide val = Map.insert ide (Stored val) memory
            
search :: Memory -> Ide -> MemVal
search memory ide =
  case Map.lookup ide memory of
    Just (Stored v) -> Stored v
    _ -> Unbound

-- Display the memory
display :: Memory -> String
display m = "Memory:\n" ++ show (Map.toList m)

-- Print the state
printState :: State -> String
printState (m, i, o) = display m ++ "Input: " ++ show i ++ "Output: " ++ show o

 -- Semantic function definition
data ExpVal = InterpOk Value State | InterpError String
exp_semantics :: Exp -> State -> ExpVal

-- Semantic fuction declaration
exp_semantics (Number num) state = InterpOk (Numeric num) state
exp_semantics (Bool bool) state = InterpOk (Boolean bool) state

exp_semantics Read (mem, [], output) = InterpError "No Input provided"
exp_semantics Read (mem, i:is, output) = InterpOk i (mem, is, output)

exp_semantics (I ide) (memory, input, output) =
    case (search memory ide) of
      Stored val -> InterpOk val (memory, input, output)
      Unbound -> InterpError $ "Identifier " ++ ide ++ " not defined."

exp_semantics (Not exp) state =
    case (exp_semantics exp state) of
      InterpOk (Boolean val) state -> InterpOk (Boolean (not val)) state
      InterpOk (Numeric _) state   ->
          InterpError "'not' can only be applied to boolean values"
      _ -> InterpError "Invalid operand for 'not'"
                  
exp_semantics (Equal exp1 exp2) state =
    case (exp_semantics exp1 state, exp_semantics exp2 state) of
      (InterpOk val1 state1, InterpOk val2 state2) ->
          InterpOk (Boolean (val1 == val2)) state
      _ -> InterpError
           "Equal Operation Requires Both Operands To Be Of Same Type"

exp_semantics (Plus exp1 exp2) state =
    case (exp_semantics exp1 state, exp_semantics exp2 state) of
      (InterpOk (Numeric num1) state1, InterpOk (Numeric num2) state2) ->
          InterpOk (Numeric (num1 + num2)) state
      _ -> InterpError "Plus Operation Requires Both Operands to be Numeric"
          

exp_semantics (Minus exp1 exp2) state =
    case (exp_semantics exp1 state, exp_semantics exp2 state) of
      (InterpOk (Numeric num1) state1, InterpOk (Numeric num2) state2) ->
          InterpOk (Numeric (num1 - num2)) state
      _ -> InterpError "Plus Operation Requires Both Operands to be Numeric"

-- Expressions that change state
--------------------------------

-- Doesn't change state
-- Returns None
exp_semantics Skip s = InterpOk None s

-- Updates Memory
-- Returns the Assignment Value (like x = 5 in C returns 5)
exp_semantics (Assign ide exp) state =
  case (exp_semantics exp state) of
    InterpOk val (mem, input, output) ->
        InterpOk val (update mem ide val, input, output)
    InterpError msg -> InterpError $ "Assignment failed: " ++ msg

-- Updates Output
-- Return the Output Value (like console.log in JS returns the value)
exp_semantics (Output exp) state =
    case (exp_semantics exp state) of
      InterpOk val (mem, input, output) ->
          InterpOk val (mem, input, output ++ [val])
      InterpError msg -> InterpError $ "Output failed: " ++ msg

-- Executes Multiple Expressions
-- Return Last Expression Value (expr1, expr2 in C returns expr2's value)
exp_semantics (Seq exp1 exp2) state =
    case (exp_semantics exp1 state) of
      InterpOk _ state1 -> exp_semantics exp2 state1
      InterpError msg -> InterpError msg

-- Repeats Execution as Long as exp1 is True
-- Return Value None (control structure, no meaningful value)
exp_semantics (WhileDo exp1 exp2) state =
    case (exp_semantics exp1 state) of
      InterpOk (Boolean True) state1 ->
        case (exp_semantics exp2 state1) of
          InterpOk val state2 -> exp_semantics (WhileDo exp1 exp2) state2
          InterpError msg -> InterpError msg
      InterpOk (Boolean False) state1 -> InterpOk None state1
      InterpOk val _ -> InterpError $
        "Expected Boolean in while condition, got: " ++ show val
      InterpError msg -> InterpError $ "While condition failed: " ++ msg

exp_semantics (IfThenElse exp1 exp2 exp3) state =
  case (exp_semantics exp1 state) of
    InterpOk (Boolean True) state1 -> exp_semantics exp2 state1
    InterpOk (Boolean False) state1 -> exp_semantics exp3 state1
    InterpError msg ->
      InterpError $ "IfThenElse requires a boolean condition: " ++ msg

-- Run the Program with the given input
run :: String -> [Value] -> [Value]
run program input =
    case eparse program of
      ParseOk parsed_program ->
          case exp_semantics parsed_program (emptyMem, input, []) of
            InterpOk _ (_, _, output') -> output'
            InterpError msg -> [Error ("Interpreter Error: " ++ msg)]
      ParseError msg -> [Error ("Parser Error: " ++ msg)] 
              
-------------------------------------------------------------------------------
-- Repl.hs
-------------------------------------------------------------------------------

getOutput :: State -> [Value]
getOutput (_, _, output') = output'

-- Read Eval Print Loop
read_ :: IO String
read_ = putStr "(^._.^) >>"
        >> hFlush stdout
        >> getLine

print_ :: String -> IO ()
print_ = putStrLn

eval_ :: String -> State -> (String, State)
eval_ program state =
    case eparse program of
      ParseOk parsed_program ->
          case exp_semantics parsed_program state of
            InterpOk val newState -> (
              "Value: "  ++ show val ++ "\n" ++
              "Output: " ++ show (getOutput newState), newState
              )
            InterpError msg -> ("Evaluation Failed. \n" ++ msg, state)
      ParseError msg -> (msg, state)
         
loop_ :: State -> IO ()
loop_ state = do
  input <- read_
  case input of
    ":quit" -> print_ "Bye!"
    ":q"    -> print_ "Bye!"
    ":mem"  -> do
      let (m, _, _) = state
      print_ (display m)
      loop_ state
    ":reset" -> do
      print_ "State reset."
      loop_ (emptyMem, [], [])
    ":help" -> do
      print_ "Available commands: :quit, :q, :mem, :reset, :help"
      loop_ state
    _ -> do
      let (output, newState) = eval_ input state
      print_ output
      loop_ newState

repl :: IO ()
repl = do
  putStrLn "Welcome to the Phoebe REPL"
  putStrLn "Type :quit or :q to exit"
  loop_ (emptyMem, [], [])
