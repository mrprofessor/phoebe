{-# LANGUAGE LambdaCase #-}
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
  -- Commands
  | Skip
  | Output Exp
  | Assign Ide Exp
  | Seq Exp Exp
  | WhileDo Exp Exp
  | IfThenElse Exp Exp Exp
  -- Declarations
  | Variable Ide Exp
  | Constant Ide Exp
  | Procedure Ide [Ide] Exp
  | ProcCall Ide [Exp] 
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
  do symbol "var"
     id <- token identifier
     symbol "="
     e <- expr
     return (Variable id e)
  +++
  do symbol "const"
     id <- token identifier
     symbol "="
     e <- expr
     return (Constant id e)
  +++
  do symbol "proc"
     name <- token identifier
     symbol "("
     params <- identifier `sepby` (symbol ",")
     symbol ")"
     symbol "->"
     body <- expr
     return (Procedure name params body)
  +++
  do symbol "proc"
     name <- token identifier
     symbol "("
     params <- identifier `sepby` (symbol ",")
     symbol ")"
     symbol "->"
     body <- expr
     return (Procedure name params body)
  +++
  do id <- token identifier
     symbol "("
     args <- expr `sepby` (symbol ",")
     symbol ")"
     return (ProcCall id args)
  +++
  do symbol "while"
     cond <- expr
     symbol "do"
     action <- expr
     return (WhileDo cond action)
  +++
  do symbol "if"
     cond <- expr
     symbol "then"
     thenExp <- expr
     symbol "else"
     elseExp <- expr
     return (IfThenElse cond thenExp elseExp)
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

data EnvVal
  = LRef Loc            -- For variables referencing a location in the Store
  | CVal Value          -- For constants holding direct immutable values
  | FDef [Ide] Exp Env  -- For functions or procedures with parameters and a body
  deriving (Show)

-- Environment & Store
type Env = Map Ide EnvVal             -- Maps identifiers to variables, constants, or functions
type Store = Map Loc Value            -- Maps locations to their associated values
type Loc = Integer                    -- Concrete representation of locations
type Input = [Value]                  -- Input stream for `read`
type Output = [Value]                 -- Output stream for `output`
type State = (Env, Store, Input, Output)

-- Initialize Env & Store
emptyEnv :: Env
emptyEnv = Map.empty

emptyStore :: Store
emptyStore = Map.empty

nextLoc :: Store -> Loc
nextLoc store = if Map.null store then 0 else maximum (Map.keys store) + 1

updateEnv :: Env -> Ide -> EnvVal -> Env
updateEnv env ide val = Map.insert ide val env

updateStore :: Store -> Loc -> Value -> Store
updateStore store loc val = Map.insert loc val store

searchStore :: Store -> Loc -> Maybe Value
searchStore store loc = Map.lookup loc store

lookupEnv :: Env -> Store -> Ide -> Maybe Value
lookupEnv env store ide =
  case Map.lookup ide env of
    Just (LRef loc)  -> searchStore store loc -- Variable: dereference location
    Just (CVal val)  -> Just val              -- Constant: direct value
    Nothing          -> Nothing

-- Print the state
display :: State -> String
display (env, store, input, output) = 
  "Environment:\n" ++ show (Map.toList env) ++ 
  "\nStore:\n" ++ show (Map.toList store) ++
  "\nInput:\n" ++ show input ++
  "\nOutput:\n" ++ show output

-- Initialize a new variable in the Env and Store
initVar :: Env -> Store -> Ide -> Value -> (Env, Store, Loc)
initVar env store ide val = (newEnv, newStore, newLoc)
  where
    newLoc = nextLoc store
    newEnv = updateEnv env ide (LRef newLoc)
    newStore = updateStore store newLoc val

-- Add a constant to Env
initConst :: Env -> Ide -> Value -> Env
initConst env ide val = updateEnv env ide (CVal val)

-- Add a procedure or function to Env
initFunc :: Env -> Ide -> [Ide] -> Exp -> Env
initFunc env ide params body = updateEnv env ide (FDef params body env)

-- Update the value of a variable in Store
updateVar :: Env -> Store -> Ide -> Value -> Maybe Store
updateVar env store ide val =
  case Map.lookup ide env of
    Just (LRef loc) -> Just (updateStore store loc val) -- Update variable's location
    _               -> Nothing                          -- Not a variable


 -- Semantic function definition
data ExpVal = InterpOk Value State | InterpError String
exp_semantics :: Exp -> State -> ExpVal

-- Semantic fuction declaration
exp_semantics (Number num) state = InterpOk (Numeric num) state
exp_semantics (Bool bool) state = InterpOk (Boolean bool) state

exp_semantics Read (env, store, [], output) = InterpError "No input provided"
exp_semantics Read (env, store, (i:is), output) =
  InterpOk i (env, store, is, output)

exp_semantics (I ide) (env, store, input, output) =
  case lookupEnv env store ide of
    Just val -> InterpOk val (env, store, input, output)
    Nothing  -> InterpError $ "Identifier " ++ ide ++ " not defined."

exp_semantics (Not exp) state =
  case (exp_semantics exp state) of
    InterpOk (Boolean val) state -> InterpOk (Boolean (not val)) state
    InterpOk _ _ ->
        InterpError "'not' can only be applied to boolean values"
    _ -> InterpError "Invalid operand for 'not'"

exp_semantics (Equal exp1 exp2) state =
  case (exp_semantics exp1 state, exp_semantics exp2 state) of
    (InterpOk val1 _, InterpOk val2 _) ->
        InterpOk (Boolean (val1 == val2)) state
    _ ->
      InterpError "Equal Operation Requires Both Operands To Be Of Same Type"

exp_semantics (Plus exp1 exp2) state =
    case (exp_semantics exp1 state, exp_semantics exp2 state) of
      (InterpOk (Numeric num1) _, InterpOk (Numeric num2) _) ->
          InterpOk (Numeric (num1 + num2)) state
      _ -> InterpError "Plus Operation Requires Both Operands to be Numeric"
          
exp_semantics (Minus exp1 exp2) state =
    case (exp_semantics exp1 state, exp_semantics exp2 state) of
      (InterpOk (Numeric num1) _, InterpOk (Numeric num2) _) ->
          InterpOk (Numeric (num1 - num2)) state
      _ -> InterpError "Plus Operation Requires Both Operands to be Numeric"

-- Expressions that change state
--------------------------------

-- Doesn't change state
-- Returns None
exp_semantics Skip s = InterpOk None s

-- Updates Env
-- Returns the Assignment Value (like x = 5 in C returns 5)
exp_semantics (Assign ide exp) (env, store, input, output) =
  case Map.lookup ide env of
    Just (LRef loc) ->
      case exp_semantics exp (env, store, input, output) of
        InterpOk val (_, newStore, _, _) ->
          InterpOk val (env, updateStore newStore loc val, input, output)
        InterpError msg ->
          InterpError $ "Assignment evaluation failed: " ++ msg
    Just (CVal _) ->
      InterpError $ "Assignment failed: " ++ ide ++ " is a constant."
    Nothing ->
      InterpError $ "Assignment failed: Identifier " ++ ide ++ " is not declared."

-- Updates Env
-- Returns the Variable Value (var x = 5 return 5)
exp_semantics (Variable ide exp) (env, store, input, output) =
  case (exp_semantics exp (env, store, input, output)) of
    InterpOk val (_, newStore, _, _) ->
      if Map.member ide env
      then InterpError $ ide ++ " is already declared."
      else
        let (newEnv, updatedStore, _) = initVar env newStore ide val
        in InterpOk val (newEnv, updatedStore, input, output)
    InterpError msg ->
      InterpError $ "Variable declaration failed: " ++ msg

-- Updates Env
-- Returns the Variable Value (const x = 5 return 5)
exp_semantics (Constant ide exp) (env, store, input, output) =
  case (exp_semantics exp (env, store, input, output)) of
    InterpOk val _ ->
      if Map.member ide env
      then InterpError $ ide ++ " is already declared."
      else InterpOk val (initConst env ide val, store, input, output)
    InterpError msg ->
      InterpError $ "Constant declaration failed: " ++ msg

-- Updates Output
-- Return the Output Value (like console.log in JS returns the value)
exp_semantics (Output exp) state =
  case (exp_semantics exp state) of
    InterpOk val (env, store, input, output) ->
      InterpOk val (env, store, input, output ++ [val])
    InterpError msg ->
      InterpError $ "Output failed: " ++ msg

-- Executes Multiple Expressions
-- Return Last Expression Value (expr1, expr2 in C returns expr2's value)
exp_semantics (Seq exp1 exp2) state =
  case (exp_semantics exp1 state) of
    InterpOk _ state' -> exp_semantics exp2 state'
    InterpError msg -> InterpError msg

-- Repeats Execution as Long as exp1 is True
-- Return Value None (control structure, no meaningful value)
exp_semantics (WhileDo cond body) state =
  case (exp_semantics cond state) of
    InterpOk (Boolean True) state' ->
      case (exp_semantics body state') of
        InterpOk _ state'' -> exp_semantics (WhileDo cond body) state''
        InterpError msg -> InterpError msg
    InterpOk (Boolean False) state' -> InterpOk None state'
    InterpOk val _ ->
      InterpError $ "Expected Boolean in while condition, got: " ++ show val
    InterpError msg -> InterpError $ "While condition failed: " ++ msg

exp_semantics (IfThenElse cond thenExp elseExp) state =
  case (exp_semantics cond state) of
    InterpOk (Boolean True) state' -> exp_semantics thenExp state'
    InterpOk (Boolean False) state' -> exp_semantics elseExp state'
    InterpOk val state' ->
      InterpError $ "Expected Boolean in while condition, got: " ++ show val
    InterpError msg ->
      InterpError $ "IfThenElse condition failed: " ++ msg

-- Procedure Declaration
exp_semantics (Procedure ide params body) (env, store, input, output) =
  if Map.member ide env then
    InterpError $ ide ++ " is already declared."
  else
    let procDef = FDef params body env
        newEnv = updateEnv env ide procDef
    in InterpOk None (newEnv, store, input, output)

-- Procedure Call to implement dynamic binding
exp_semantics (ProcCall ide args) state@(env, store, input, output) =
  case Map.lookup ide env of
    Just (FDef params body callEnv)
      | length params /= length args ->
          InterpError $ "Procedure " ++ ide ++ " called with wrong number of arguments."
      | otherwise -> 
          -- Evaluate all arguments
          let evalResults = map (`exp_semantics` state) args
              evalErrors = [msg | InterpError msg <- evalResults]
          in if not (null evalErrors) then
               InterpError $ "Argument evaluation failed: " ++ unlines evalErrors
             else
               let argVals = [val | InterpOk val _ <- evalResults] -- Extract evaluated values
                   newLocs = [nextLoc store + fromIntegral i | i <- [0 .. length argVals - 1]]
                   newStore = foldr (uncurry Map.insert) store (zip newLocs argVals) -- Update store
                   paramBindings = zip params (map LRef newLocs) -- Create parameter bindings
                   updatedCallEnv = foldr (uncurry Map.insert) callEnv paramBindings -- Extend procedure environment
                   newEnv = Map.insert ide (FDef params body updatedCallEnv) env -- Update global environment
               in case exp_semantics body (updatedCallEnv, newStore, input, output) of
                    InterpOk val (_, finalStore, finalInput, finalOutput) ->
                      InterpOk val (newEnv, finalStore, finalInput, finalOutput)
                    InterpError msg -> InterpError msg
    _ ->
      InterpError $ "Procedure " ++ ide ++ " is not defined."

run :: String -> [Value] -> [Value]
run program input =
    case eparse program of
      ParseOk parsed_program ->
          case exp_semantics parsed_program (emptyEnv, emptyStore, input, []) of
            InterpOk _ (_, _, _, output') -> output'
            InterpError msg -> [Error ("Interpreter Error: " ++ msg)]
      ParseError msg -> [Error ("Parser Error: " ++ msg)]

-------------------------------------------------------------------------------
-- Repl.hs
-------------------------------------------------------------------------------

getOutput :: State -> [Value]
getOutput (_, _, _, output') = output'

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
    ":env"  -> do
      let (env, store, _, _) = state
      print_ "Environment:"
      print_ (show $ Map.toList env)
      print_ "Store:"
      print_ (show $ Map.toList store)
      loop_ state
    ":reset" -> do
      print_ "State reset."
      loop_ (emptyEnv, emptyStore, [], [])
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
  loop_ (emptyEnv, emptyStore, [], [])
