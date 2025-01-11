module Interpreter where
import Parsing
import System.IO (hFlush, stdout)
import Debug.Trace (trace) -- For debugging
import System.Environment (getArgs)

-------------------------------------------------------------------------------
-- PARSER
-------------------------------------------------------------------------------

type Ide = String

-- P ::= program C
data Program = Program Com
  deriving Show

-- E :: = B | true | false | read | I | E1 (E2) | if E then E1 else E2 | E O E2
data Exp
  = Number Integer
  | Bool Bool
  | Read
  | Identifier Ide
  | CallFun Ide [Exp]                 -- Later support CallFun Exp [Exp]
  | IfExp Exp Exp Exp
  | BinOp String Exp Exp
  deriving Show

-- C::= E1 := E2 | output E | E1(E2) | if E then C1 else C2 | while E do C | begin D;C end | C1 ;C2
data Com
  = Assign Exp Exp
  | Output Exp
  | CallProc Ide [Exp]                -- Later support CallProc Exp [Exp]
  | IfCom Exp Com Com
  | WhileDo Exp Com 
  | BeginEnd Dec Com
  | ComSeq Com Com
  deriving Show

-- D ::= const I = E | var I = E | proc I(I1),C | fun I(I1),E | D1;D2
data Dec
  = Constant Ide Exp
  | Variable Ide Exp
  | Procedure Ide [Ide] Com           -- Regular procedure
  | RecProcedure Ide [Ide] Com        -- Recursive procedure
  | Function Ide [Ide] Exp            -- Regular function
  | RecFunction Ide [Ide] Exp         -- Recursive function
  | DecSeq Dec Dec
  deriving Show

data ParsedResult
  = ParseOk Program
  | ParseError String
  deriving Show

binop :: Parser String
binop = do   symbol "+"   ; return "+"
      +++ do symbol "-"   ; return "-"
      +++ do symbol "*"   ; return "*"
      +++ do symbol "/"   ; return "/"
      +++ do symbol "=="  ; return "=="
      +++ do symbol "<"   ; return "<"
      +++ do symbol ">"   ; return ">"

expr :: Parser Exp
expr = 
  do e1 <- term
     op <- binop
     e2 <- expr
     return (BinOp op e1 e2)
  +++
  do symbol "if"
     cond <- expr
     symbol "then"
     thenCom <- expr
     symbol "else"
     elseCom <- expr
     return (IfExp cond thenCom elseCom)
  +++
  do name <- token identifier
     symbol "!"
     symbol "("
     args <- expr `sepby` (symbol ",")
     symbol ")"
     return (CallFun name args)
  +++
  term

term :: Parser Exp
term = factor

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
  do symbol "read"
     return Read
  +++
  do id <- token identifier
     return (Identifier id)
  +++
  do symbol "("
     e <- expr
     symbol ")"
     return e

cmd :: Parser Com
cmd =
  do symbol "output"
     e <- expr
     return (Output e)
  +++
  do leftExp <- expr
     symbol ":="
     rightExp <- expr
     return (Assign leftExp rightExp)
  +++
  do symbol "begin"
     decs <- decSeq
     symbol ";"                       -- Mandatory ; after declarations
     cmds <- cmdSeq
     symbol "end"
     return (BeginEnd decs cmds)
  +++
  do symbol "while"
     cond <- expr
     symbol "do"
     action <- cmd
     return (WhileDo cond action)
  +++
  do symbol "if"
     cond <- expr
     symbol "then"
     thenCom <- cmd
     symbol "else"
     elseCom <- cmd
     return (IfCom cond thenCom elseCom)
  +++
  do name <- token identifier
     symbol "("
     args <- expr `sepby` (symbol ",")
     symbol ")"
     return (CallProc name args)
  +++
  do symbol "{"
     c <- cmdSeq
     symbol "}"
     return c

-- Sequence of commands
cmdSeq :: Parser Com
cmdSeq =
  do c1 <- cmd
     (do symbol ";"
         rest <- cmdSeq
         return (ComSeq c1 rest)
      -- +++                          -- Make ; optional for the last command
      -- do symbol ";"
      --    return c1)
      +++ 
      return c1)

-- Declaration parsers
dec :: Parser Dec
dec =
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
     symbol ","
     body <- cmd
     return (Procedure name params body)
  +++
  do symbol "fun"
     name <- token identifier
     symbol "("
     params <- identifier `sepby` (symbol ",")
     symbol ")"
     symbol ","
     body <- expr          -- Functions return expressions, not commands
     return (Function name params body)
  +++
  do symbol "rec"          -- Recursive procedure
     symbol "proc"
     name <- token identifier
     symbol "("
     params <- identifier `sepby` (symbol ",")
     symbol ")"
     symbol ","
     body <- cmd
     return (RecProcedure name params body)
  +++
  do symbol "rec"          -- Recursive function
     symbol "fun"
     name <- token identifier
     symbol "("
     params <- identifier `sepby` (symbol ",")
     symbol ")"
     symbol ","
     body <- expr
     return (RecFunction name params body)

-- Sequence of Declarations
decSeq :: Parser Dec
decSeq =
  do d1 <- dec
     (do symbol ";"
         rest <- decSeq
         return (DecSeq d1 rest)
      -- +++                          -- Make ; optional for the last dec
      -- do symbol ";"
      --    return d1
      +++
      return d1)

-- Program parser
-- Program ::= program C
program :: Parser Program
program =
  do symbol "program"
     c <- cmd
     return (Program c)


-- Main parse function
sparse :: String -> ParsedResult
sparse xs = case Parsing.parse program xs of
             [(result, [])]   -> ParseOk result
             [(result, out_)] -> ParseError ("Unused input " ++ out_)
             []               -> ParseError "Invalid input"



-------------------------------------------------------------------------------
-- INTERPRETER
-------------------------------------------------------------------------------


-- Value and Semantic Domains
-- ----------------------------------------------------------------------------

-- Value Domain: Dv = Loc + Rv + Proc + Fun
-- Rv = Bool + Bv (basic values and booleans)
data Value 
  = Numeric Integer              -- Represents basic values (Bv)
  | Boolean Bool                 -- Represents boolean values (Bool)
  | File [Value]                 -- Represents files (File = Rv*)
  | Unused                       -- Represents {unused} in Store
  | Error String                 -- Represents error states
  deriving (Eq, Show)

-- Denotable Values: Dv = Loc + Rv + Proc + Fun
data EnvVal
  = LocRef Integer               -- Represents locations (Loc)
  | ConstVal Value               -- Represents constant values (Rv)
  | ProcDef [Ide] Com Env        -- Represents procedures (Proc)
  | FunDef [Ide] Exp Env         -- Represents functions (Fun)
  | Unbound                      -- Represents unbound identifiers

-- Semantic Domains
-- ----------------------------------------------------------------------------

-- Environment Domain: Env = Ide -> [Dv + {unbound}]
type Env = Ide -> EnvVal

-- Store Domain: Store = Loc -> [Sv + {unused}]
type Store = Integer -> Value

-- Input/Output Files: File = Rv*
type File = [Value]
type Input = [Value]
type Output = [Value]
type NextLoc = Integer

-- State: Captures the current env, store, next location, input, and output
type State = (Env, Store, NextLoc, Input, Input)

-- Semantic Domain: Ans = {error, stop} + [Rv x Ans]
data Ans
  = Stop State                   -- Final state of the program
  | ErrorState String            -- Represents errors in execution
  | Result [Value] State         -- Result with continuation (list of R-values)

-- Mix Functions/Procedure type
type Mix = Env -> Env -> Env

-- λ(r,r') . r
staticMix :: Mix
staticMix declEnv callEnv = declEnv

-- λ(r,r') . r'
dynamicMix :: Mix
dynamicMix declEnv callEnv = callEnv

-- Continuations
-- ----------------------------------------------------------------------------

type Ec = Value -> State -> Ans  -- Ec = Val -> State -> Ans
type Cc = State -> Ans           -- Cc = State -> Ans
type Dc = Env -> Cc              -- Dc = Env -> Cc

-- Initialize Environment
-- ----------------------------------------------------------------------------

-- Default environment returns Unbound for all identifiers
defaultEnv :: Env
defaultEnv _ = Unbound

-- Default store returns Unused for all locations
defaultStore :: Store
defaultStore _ = Unused

-- Initialize the state with input
initState :: [Value] -> State
initState inputs = (defaultEnv, defaultStore, 0, inputs, [])

-- Helper Functions
-- ----------------------------------------------------------------------------

-- Helper function to update the store
updateStore :: Integer -> Value -> Store -> Store
updateStore loc newVal store = \x -> if x == loc then newVal else store x

-- Evaluate arguments (helper function)
evalArgs :: [Exp] -> Env -> State -> ([Value] -> State -> Ans) -> Ans
evalArgs [] _ state k = k [] state -- No arguments
evalArgs (e:es) env state k =
  exp_semantics e env (\v state' ->
    evalArgs es env state' (\vs state'' ->
      k (v:vs) state''
    )
  ) state

-- Extend the environment with a parameter and its value
extendEnv :: Ide -> EnvVal -> Env -> Env
extendEnv ide envVal env = \x -> if x == ide then envVal else env x

-- Helper function for binding parameters to new locations
bindParameters ::
    [Ide] -> [Value] -> Env -> Store -> NextLoc -> (Env, Store, NextLoc)
bindParameters paramNames argVals env store nextLoc =
  foldl (\(e, s, n) (param, val) ->
          let newStore = updateStore n val s
              newEnv = extendEnv param (LocRef n) e
          in (newEnv, newStore, n + 1))
        (env, store, nextLoc)
        (zip paramNames argVals)

-- Expression Semantics (E)
-- ----------------------------------------------------------------------------

-- E: Exp -> Env -> Ec -> Cc
exp_semantics :: Exp -> Env -> Ec -> State -> Ans

-- E[B] r k = k B[B]
exp_semantics (Number num) env k state = k (Numeric num) state

-- E[true] r k = k true, E[false] r k = k false
exp_semantics (Bool bool) env k state = k (Boolean bool) state

-- E[read] r k s =
--   null(s input) -> error
--   Otherwise k (head(s input)) (s[tail(s input)/input])
exp_semantics Read _ k (e, s, nl, [], o) = ErrorState "No Input Provided"
exp_semantics Read _ k (e, s, nl, i:is, o) = k i (e, s, nl, is, o)

-- E[I] r k =
--   (r I = unbound) -> err
--   Otherwise -> k (r I)
exp_semantics (Identifier ide) env k state@(env', store, nl, input, output) =
  case env ide of
    Unbound -> ErrorState $ "Undefined identifier: " ++ ide
    LocRef loc -> case store loc of
      Unused -> ErrorState $ "Accessed unused memory location for: " ++ ide
      val    -> k val state
    ConstVal val -> k val state
    _ -> ErrorState $ "Unexpected environment value for: " ++ ide

-- E[E1(E2)] r k = E[E1] r; Fun?λf.E[E2] r; f; k
exp_semantics (CallFun funName args) callEnv k state =
  case callEnv funName of
    FunDef paramNames body declEnv ->
      if length paramNames /= length args then
        ErrorState $ "Argument count mismatch for function: " ++ funName
      else
        -- Evaluate the args
        evalArgs args callEnv state
          (\argVals state'@(_, store', nl', input', output') ->
            -- Use either declEnv or callEnv environment for the function body
            let preferredEnv = dynamicMix declEnv callEnv
                (finalEnv, finalStore, finalNl) =
                  -- Extend preferred environment with parameters
                  bindParameters paramNames argVals preferredEnv store' nl'
            in exp_semantics body finalEnv k
                 (finalEnv, finalStore, finalNl, input', output'))
    _ -> ErrorState $ funName ++ " is not a Function."


-- E[if E then E, else E2] r k = R[E] r; Bool?; cond(E[E1] r k, E[E2] r k)
exp_semantics (IfExp cond thenExp elseExp) env k state =
  exp_semantics cond env (\v state' ->
    case v of
      Boolean True -> exp_semantics thenExp env k state'
      Boolean False -> exp_semantics elseExp env k state'
      _ -> ErrorState "IfThenElse condition must evaluate to a boolean"
    ) state

-- E[E1 O E2] r k = R[E1] r λe1. R[E2] r λe2. O[O](e1, e2) k
exp_semantics (BinOp op exp1 exp2) env k state =
  exp_semantics exp1 env (\v' state' ->
    exp_semantics exp2 env (\v'' state'' ->
      case (v', v'') of
        (Numeric n1, Numeric n2) -> 
          let result =  case op of
                "+" ->  Numeric (n1 + n2)
                "-" ->  Numeric (n1 - n2)
                "*" ->  Numeric (n1 * n2)
                "/" ->  if n2 /= 0
                        then Numeric (n1 `div` n2)
                        else Error "Division by zero"
                "==" -> Boolean (n1 == n2)
                "<" ->  Boolean (n1 < n2)
                ">" ->  Boolean (n1 > n2)
                _   ->  Error $ "Unknown operator: " ++ op
          in case result of
               Error err -> ErrorState err
               res       -> k res state''

        (Boolean b1, Boolean b2) ->
          let result = case op of
                ">" ->  Boolean (b1 && b2)
                "<" ->  Boolean (b1 || b2)
                "==" -> Boolean (b1 == b2)
                _    -> Error $ "Unknown operator: " ++ op
          in case result of
               Error err -> ErrorState err
               res       -> k res state''

        _ -> ErrorState $
             "Type mismatch : " ++ show v' ++ " " ++ op ++ " " ++ show v''
    ) state'
  ) state


-- Command Semantics (C)
-- ----------------------------------------------------------------------------


-- C :: Com -> Env -> Cc -> Cc
com_semantics :: Com -> Env -> Cc -> State -> Ans

-- (C1) Assignment:
-- C[E1 := E2] r c = R[E1] r ; Loc? ; λl.(R[E2] r ; update l ; c)
com_semantics (Assign e1 e2) env k state =
  case e1 of
    Identifier ide -> -- The left-hand side must be an identifier FIXME
      case env ide of
        LocRef loc -> -- Check if the identifier maps to a location
          exp_semantics e2 env (\v2 state' ->
            let (env', store, nl, input, output) = state'
                updatedStore = updateStore loc v2 store
            in k (env', updatedStore, nl, input, output)
          ) state
        _ -> ErrorState
             $ "Left-hand side of assignment must be a variable or location: "
             ++ ide
    _ -> ErrorState "Left-hand side of assignment must be an identifier"
    
-- (C2) Output:
-- C[output E] r c = R[E] r λe s. (e, s)
com_semantics (Output e) env k state =
  exp_semantics e env (\v state'@(env', store, nl, input, output) ->
    k (env', store, nl, input, output ++ [v])
  ) state

-- (C3) Procedure Call:
-- C[E1(E2)] r c = E[E1] r ; Proc? λp . E[E2] r ; p ; c
com_semantics (CallProc procName args) callEnv k state =
  case callEnv procName of
    ProcDef paramNames body declEnv ->
      if length paramNames /= length args then
        ErrorState $ "Argument count mismatch for procedure: " ++ procName
      else
        evalArgs args callEnv state
          (\argVals state'@(_, store', nl', input', output') ->
            -- Use either declEnv or callEnv environment for the procedure body
            let preferredEnv = dynamicMix declEnv callEnv
                -- Extend preferred environment with parameters
                (finalEnv, finalStore, finalNl) =
                  bindParameters paramNames argVals preferredEnv store' nl'
            in com_semantics body finalEnv k
                 (finalEnv, finalStore, finalNl, input', output'))
    _ -> ErrorState $ procName ++ " is not a procedure"

-- (C4) Conditional: 
-- C[if E then C1 else C2] r c = R[E] r ; Bool? ; cond(C[C1] r c, C[C2] r c)
com_semantics (IfCom cond thenCom elseCom) env k state =
  exp_semantics cond env (\v state' ->
    case v of
      Boolean True -> com_semantics thenCom env k state'
      Boolean False -> com_semantics elseCom env k state'
      _ -> ErrorState $ "Expected a boolean in if condition, got: " ++ show v
  ) state

-- (C5) While Loop:
-- C[while E do C] r c = R[E] r ; Bool? ; cond(C[C] r (C[while E do C] r c), c)
com_semantics (WhileDo cond body) env k state = 
  exp_semantics cond env (\v state' ->
    case v of
      Boolean True -> com_semantics body env (\state'' -> 
        com_semantics (WhileDo cond body) env k state'') state'
      Boolean False -> k state'
      _ -> ErrorState $ "Expected a boolean in while condition, got: " ++ show v
  ) state

-- (C6) Block:
-- C[begin D;C end] r c = D[D] r λr'.C[C] r[r'] c
com_semantics (BeginEnd decs cmds) env k state =
  dec_semantics decs env (\extendedEnv ->
    com_semantics cmds extendedEnv k
  ) state

-- (C7) Sequence:
-- C[C1;C2] r c = C[C1] r ; C[C2] r ; c
com_semantics (ComSeq c1 c2) env k state =
  com_semantics c1 env (\state' ->
    com_semantics c2 env k state'
  ) state


-- Declaration Semantics (D)
-- ----------------------------------------------------------------------------


-- D: Dec -> Env -> Dc -> State -> Ans
dec_semantics :: Dec -> Env -> Dc -> State -> Ans

-- (D1) Constant declaration:
-- D[const I = E] r u = R[E] r λe . u(e/I)
dec_semantics (Constant ide exp) env k state =
  exp_semantics exp env (\val state' ->
    case val of
      Error _ -> ErrorState $ "Invalid constant value for " ++ ide
      _       -> let extendedEnv = extendEnv ide (ConstVal val) env
                 in k extendedEnv state'
  ) state

-- (D2) Variable declaration:
-- D[var I = E] r u = R[E] r; ref λi . u(i/I)
dec_semantics (Variable ide exp) env k state@(e, s, nl, i, o) = 
  exp_semantics exp env (\val (e', s', nl', i', o') -> 
    let newStore = updateStore nl' val s'
        extendedEnv = extendEnv ide (LocRef nl') env
    in
       k extendedEnv (e', newStore, nl' + 1, i', o')
  ) state

-- (D3) Procedure declaration:
-- D[proc I(I1); C] r u = u((λc e . C[C] r[e/I1] c)/I)
dec_semantics (Procedure ide params body) env k state =
  let procDef = ProcDef params body env
      extendedEnv = extendEnv ide procDef env
  in k extendedEnv state

-- Recursive Procedure
dec_semantics (RecProcedure ide params body) env k state =
  let procDef = ProcDef params body env'
      env' = extendEnv ide procDef env      -- lazy recursive binding of env'
  in k env' state


-- (D4) Function declaration:
-- D[fun I(I1); E] r u = u((λk e . E[E] r[e/I1] k)/I)
dec_semantics (Function ide params body) env k state =
  let funcDef = FunDef params body env
      extendedEnv = extendEnv ide funcDef env
  in
    k extendedEnv state

-- Recursive Function
dec_semantics (RecFunction ide params body) env k state =
    let funcDef = FunDef params body env'
        env' = extendEnv ide funcDef env
    in
      k env' state

-- (D5) Sequence of declarations:
-- D[D1;D2] r u = D[D1] r λr₁ . D[D2] r[r1] λr2 . u(r1[r2])
dec_semantics (DecSeq d1 d2) env k state =
  dec_semantics d1 env (\extendedEnv state' ->
    dec_semantics d2 extendedEnv k state'
  ) state


-- Run Interpreter
-- ----------------------------------------------------------------------------

run :: String -> [Value] -> Ans
run program input =
    case sparse program of
      ParseOk (Program cmd) ->
        com_semantics cmd defaultEnv Stop (initState input)
      ParseError msg -> ErrorState ("Parser Error: " ++ msg)

instance Show Ans where
  show (Stop (_, _, _, _, output)) = "Stop with output: " ++ show output
  show (ErrorState msg) = "ErrorState: " ++ msg
  show (Result values state) = "Result: " ++ show values

-- ++ ", State: " ++ show state
instance Show EnvVal where
  show (LocRef loc) = "LocRef " ++ show loc
  show (ConstVal val) = "ConstVal " ++ show val
  show (ProcDef params _ _) = "ProcDef with params " ++ show params
  show (FunDef params _ _) = "FunDef with params " ++ show params
  show Unbound = "Unbound"

-- Helper function to display environment
showEnv :: [Ide] -> Env -> String
showEnv keys env = unlines [key ++ " -> " ++ show (env key) | key <- keys]
