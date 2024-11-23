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
  | Skip
  | Output Exp
  | Assign Ide Exp
  | Seq Exp Exp
  | WhileDo Exp Exp
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
  do symbol "skip"
     return Skip
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
search memory ide = case Map.lookup ide memory of
                      Just (Stored v) -> Stored v
                      _ -> Unbound

-- Display the memory
display :: Memory -> String
display m = "Memory:\n" ++ show (Map.toList m)

-- Print the state
printState :: State -> String
printState (m, i, o) = display m ++ "Input: " ++ show i ++ "Output: " ++ show o

 -- Semantic function definition
data ExpVal = IntrpOk Value State | IntrpError String
exp_semantics :: Exp -> State -> ExpVal

-- Semantic fuction declaration
exp_semantics (Number num) state = IntrpOk (Numeric num) state
exp_semantics (Bool bool) state = IntrpOk (Boolean bool) state

exp_semantics (I ide) (memory, input, output) =
    case (search memory ide) of
      Stored val -> IntrpOk val (memory, input, output)
      Unbound -> IntrpError $ "Identifier " ++ ide ++ " not defined."

exp_semantics (Not exp) state =
    case (exp_semantics exp state) of
      IntrpOk (Boolean val) state -> IntrpOk (Boolean (not val)) state
      IntrpOk (Numeric _) state   ->
          IntrpError "'not' can only be applied to boolean values"
      _ -> IntrpError "Invalid operand for 'not'"
                  
exp_semantics (Equal exp1 exp2) state =
    case (exp_semantics exp1 state, exp_semantics exp2 state) of
      (IntrpOk val1 state1, IntrpOk val2 state2) ->
          IntrpOk (Boolean (val1 == val2)) state
      _ -> IntrpError
           "Equal Operation Requires Both Operands To Be Of Same Type"

exp_semantics (Plus exp1 exp2) state =
    case (exp_semantics exp1 state, exp_semantics exp2 state) of
      (IntrpOk (Numeric num1) state1, IntrpOk (Numeric num2) state2) ->
          IntrpOk (Numeric (num1 + num2)) state
      _ -> IntrpError "Plus Operation Requires Both Operands to be Numeric"
          

exp_semantics (Minus exp1 exp2) state =
    case (exp_semantics exp1 state, exp_semantics exp2 state) of
      (IntrpOk (Numeric num1) state1, IntrpOk (Numeric num2) state2) ->
          IntrpOk (Numeric (num1 - num2)) state
      _ -> IntrpError "Plus Operation Requires Both Operands to be Numeric"

-- Expressions that change state
--------------------------------

-- Returns None
exp_semantics Skip s = IntrpOk None s

-- Returns the Assignment Value (like x = 5 in C returns 5)
exp_semantics (Assign ide exp) state =
  case (exp_semantics exp state) of
    IntrpOk val (mem, input, output) ->
        IntrpOk val (update mem ide val, input, output)
    IntrpError msg -> IntrpError $ "Assignment failed: " ++ msg

-- Return the Output Value (like console.log in JS returns the value)
exp_semantics (Output exp) state =
    case (exp_semantics exp state) of
      IntrpOk val (mem, input, output) ->
          IntrpOk val (mem, input, output ++ [val])
      IntrpError msg -> IntrpError $ "Output failed: " ++ msg

-- Return Last Expression Value (expr1, expr2 in C returns expr2's value)
exp_semantics (Seq exp1 exp2) state =
    case (exp_semantics exp1 state) of
      IntrpOk _ state1 -> exp_semantics exp2 state1
      IntrpError msg -> IntrpError msg

-- Return Value None (control structure, no meaningful value)
exp_semantics (WhileDo exp1 exp2) state =
    case (exp_semantics exp1 state) of
      IntrpOk (Boolean True) state1 ->
        case (exp_semantics exp2 state1) of
          IntrpOk val state2 -> exp_semantics (WhileDo exp1 exp2) state2
          IntrpError msg -> IntrpError msg
      IntrpOk (Boolean False) state1 -> IntrpOk None state1
      IntrpOk val _ -> IntrpError $
        "Expected Boolean in while condition, got: " ++ show val
      IntrpError msg -> IntrpError $ "While condition failed: " ++ msg
      
 

-- Run the Program with the given input
run :: String -> [Value] -> [Value]
run program input =
    case eparse program of
      ParseOk parsed_program ->
          case exp_semantics parsed_program (emptyMem, input, []) of
            IntrpOk _ (_, _, output') -> output'
            IntrpError msg -> [Error ("Interpreter Error: " ++ msg)]
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
            IntrpOk val newState -> (
              "Value: "  ++ show val ++ "\n" ++
              "Output: " ++ show (getOutput newState), newState
              )
            IntrpError msg -> ("Evaluation Failed. \n" ++ msg, state)
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
