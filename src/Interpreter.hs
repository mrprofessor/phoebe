module Interpreter where
import Parser
import Control.Exception
import Data.List (find, intercalate)
import Debug.Trace (trace) -- For debugging
import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
-- INTERPRETER
-------------------------------------------------------------------------------


-- Value Domains
-- ----------------------------------------------------------------------------

-- Bv = Num + Bool + Str + Loc (Basic Values) (Not Required)

-- Rv = Bool + Bv (basic values and booleans)
data Value
  = Numeric Integer                    -- Represents basic values (Bv)
  | Boolean Bool                       -- Represents boolean values (Bool)
  | Str String                         -- Represents string values (String)
  | Error String                       -- Represents Errors as Value (Error)
  deriving (Eq, Show)

-- Denotable Values: Dv = Loc + Rv + Proc + Fun
data EnvVal
  = Location Integer                   -- Represents locations (Loc)
  | RValue Value                       -- Represents R-values (Rv)
  | ProcValue Proc                     -- Procedure Value Proc = Cc -> Ec
  | FunValue Fun                       -- Function Value Fun = Ec -> Ec
  | ArrayVal Integer Integer [Integer] -- Arrays (lb, ub, [location]) (Array)
  | RecordVal [Ide] Env                -- Represents records (Record)
  | LabelDef Cc                        -- Represents Label definations (Label)
  | Unbound                            -- Represents unbound identifiers

-- Sv = File + Rv
data StoreVal
  = File [Value]                       -- Represents files (File = Rv*)
  | SValue Value                       -- Represents Rv
  | ReservedArr                        -- Represents reserved array locations
  | Unused                             -- Represents {unused} in Store
  deriving Eq

-- Ev = Dv (Expressible Values e) (Not Required)

-- Semantic Domains
-- ----------------------------------------------------------------------------

type Env = Ide -> EnvVal          -- Env = Ide -> [Dv + {unbound}]
type Store = Integer -> StoreVal  -- Store = Loc -> [Sv + {unused}]

-- Ans = {error, stop} + [Rv x Ans]
data Ans
  = Stop Store                   -- Final store of the program
  | ErrorState String            -- Represents errors in execution
  | Result [Value] Ans           -- Result with continuation (list of R-values)

-- Continuation Domains (Expressions, Commands, Declarations Continuations)
type Ec = EnvVal -> Cc           -- Ec = Ev -> Cc
type Cc = Store -> Ans           -- Cc = Store -> Ans
type Dc = Env -> Cc              -- Dc = Env -> Cc

-- Procedure and Function Domains
-- type Proc = Cc -> Ec                   -- Proc = Cc -> Ec
-- type Fun  = Ec -> Ec                   -- Fun  = Ec -> Ec
-- type Proc = Cc -> [EnvVal] -> Cc       -- p = λc(e1,...,en)
-- type Fun  = Ec -> [EnvVal] -> Cc       -- f = λk(e1,...,en)

type Proc = Env -> Cc -> [EnvVal] -> Cc   -- Proc = Env -> Cc -> [Ev] -> Cc
type Fun  = Env -> Ec -> [EnvVal] -> Cc   -- Fun  = Env -> Ec -> [Ev] -> Cc

-- Mix Functions/Procedure type
type Mix = Env -> Env -> Env

-- λ(r,r') . r
staticMix :: Mix
staticMix declEnv callEnv = declEnv

-- λ(r,r') . r'
dynamicMix :: Mix
dynamicMix declEnv callEnv = callEnv


-- Initialize Environment
-- ----------------------------------------------------------------------------

inputLoc :: Integer
inputLoc = 0  -- Reserve location 0 for input

-- Initialize the store with input, Input is stored in location 0
initStore :: [Value] -> Store
initStore input = \loc ->
  if loc == inputLoc
  then File input  -- Location 0 holds the input file
  else Unused      -- All other locations are initialized as Unused

-- Initialize the enviornment with Unbound for all identifiers
initEnv :: Env
initEnv _ = Unbound


-- Expression Semantics (E)
-- ----------------------------------------------------------------------------

-- Deduction of k
-- type Cc = Store -> Ans
-- k :: EnvVal -> (Store -> Ans)
-- k EnvVal :: Store -> Ans
-- k EnvVal Store ::  Ans


-- E: Exp -> Env -> Ec -> Cc
exp_semantics :: Exp -> Env -> Ec -> Cc

-- E[B] r k = k B[B]
exp_semantics (Number num) env k = \store ->
    k (RValue (Numeric num)) store

-- E[true] r k = k true, E[false] r k = k false
exp_semantics (Bool bool) env k = \store ->
    k (RValue (Boolean bool)) store

-- E["s"] r k = k "s"
exp_semantics (String s) env k = \store ->
    k (RValue (Str s)) store

-- E[read] r k s =
--   null(s input) -> error
--   Otherwise k (head(s input)) (s[tail(s input)/input])
exp_semantics Read _ k = \store ->
    case store inputLoc of
        File [] -> ErrorState "No Input Provided"
        File (i:is) -> k (RValue i) (upsertStore inputLoc (File is) store)
        _ -> ErrorState "Invalid input file"

-- E[I] r k =
--   (r I = unbound) -> err
--   Otherwise -> k (r I)
exp_semantics (Identifier ide) env k = \store ->
  case env ide of
    Location loc -> k (Location loc) store
    RValue val -> k (RValue val) store
    ArrayVal _ _ _ -> k (env ide) store
    RecordVal _ _  -> k (env ide) store
    _ -> ErrorState $ "Unexpected environment value for: " ++ ide

-- E[E1(E2)] r k = E[E1] r; Fun?λf.E[E2] r; f; k
exp_semantics (CallFun funName args) env k = \store ->
  case env funName of
    FunValue fun ->
      evalArgs args env store [] (\vals store' ->
        fun env k vals store')
    _ -> ErrorState $ "Undefined or invalid function: " ++ funName

-- E[if E then E1 else E2] r k = R[E] r; Bool?; cond(E[E1] r k, E[E2] r k)
exp_semantics (IfExp condition thenExp elseExp) env k = \store ->
  exp_semantics condition env (\conditionVal store' ->
    case conditionVal of
      RValue (Boolean True)  -> exp_semantics thenExp env k store'
      RValue (Boolean False) -> exp_semantics elseExp env k store'
      _ -> ErrorState $ "Expected a boolean in if condition, got: "
                      ++ show conditionVal
    ) store

-- E[E1 O E2] r k = R[E1] r λe1. R[E2] r λe2. O[O](e1, e2) k
exp_semantics (BinOp op exp1 exp2) env k = \store ->
  exp_semantics exp1 env (\v' store' ->
    exp_semantics exp2 env (\v'' store'' ->
      case (deref v' store'', deref v'' store'') of
        (Numeric n1, Numeric n2) ->
          let result = case op of
                -- Comparison operators
                "<=" -> Boolean (n1 <= n2)
                ">=" -> Boolean (n1 >= n2)
                "==" -> Boolean (n1 == n2)
                "<"  -> Boolean (n1 < n2)
                ">"  -> Boolean (n1 > n2)
                -- Arithmetic operators
                "+"  -> Numeric (n1 + n2)
                "-"  -> Numeric (n1 - n2)
                "*"  -> Numeric (n1 * n2)
                "/"  -> if n2 /= 0
                       then Numeric (n1 `div` n2)
                       else Error "Division by zero"
                "%"  -> if n2 /= 0
                       then Numeric (n1 `mod` n2)
                       else Error "Modulo by zero"
                _    -> Error $ "Unknown operator: " ++ op
          in case result of
               Error err -> ErrorState err
               res       -> k (RValue res) store''

        (Boolean b1, Boolean b2) ->
          let result = case op of
                -- Boolean operators
                "==" -> Boolean (b1 == b2)
                _    -> Error $ "Unknown operator: " ++ op
          in case result of
               Error err -> ErrorState err
               res       -> k (RValue res) store''

        _ -> ErrorState $
             "Type mismatch : " ++ show v' ++ " " ++ op ++ " " ++ show v''
    ) store'
  ) store

-- Semantic function for array access:
-- E[E1[E2]] r k = E[E1] r (subscript k e_2)
exp_semantics (ArrayAccess array idx) env k = \store ->
  exp_semantics array env (\arrayVal store' ->
    exp_semantics idx env (subscript arrayVal k) store'
  ) store

-- Semantic function for record access:
exp_semantics (RecordAccess recordExp fieldExp) env k = \store ->
  exp_semantics recordExp env (\recordVal store' ->
    case recordVal of
      RecordVal _ recordEnv ->
        exp_semantics fieldExp recordEnv (\fieldVal store'' ->
          case fieldVal of
            Location loc -> k (Location loc) store''
            _ -> ErrorState "Record field must evaluate to a location"
        ) store'
      _ -> ErrorState "Not a record"
  ) store


-- Command Semantics (C)
-- ----------------------------------------------------------------------------

-- C :: Cmd -> Env -> Cc -> Cc
cmd_semantics :: Cmd -> Env -> Cc -> Cc

-- (C1) Assignment:
-- C[E1 := E2] r c = E[E1] r ; Loc? ; λl.(R[E2] r ; update l ; c)
cmd_semantics (Assign (Identifier ide) rhs) env c = \store ->  -- lhs == Ide
  case env ide of
    Location loc ->
      exp_semantics rhs env (\val2 store' ->
        case val2 of
          RValue v -> c (upsertStore loc (SValue v) store')
          Location _ -> c (upsertStore loc (SValue (deref val2 store')) store')
          _ -> ErrorState "Invalid R-value on the right-hand side"
      ) store
    _ -> ErrorState "Invalid location on the left-hand side"

-- Array element assignment
cmd_semantics (Assign (ArrayAccess array idx) rhs) env c = \store ->
  case array of
    Identifier arrName ->
      case env arrName of
        ArrayVal lb ub locs ->
          exp_semantics idx env (\idxVal store' ->
            case idxVal of
              RValue (Numeric i) ->
                if between lb ub i
                then
                    let offset = fromIntegral (i - lb)
                        loc = locs !! fromIntegral offset
                    in exp_semantics rhs env (\rhsVal store'' ->
                         case rhsVal of
                           RValue v -> c (upsertStore loc (SValue v) store'')
                           Location l -> c (upsertStore loc (store'' l) store'')
                           _ -> ErrorState "Invalid assignment value"
                          ) store'
                else ErrorState $ "Array index " ++ show i
                                ++ " out of bounds ["
                                ++ show lb ++ ".." ++ show ub ++ "]"
              _ -> ErrorState "Array index must be numeric"
            ) store
        _ -> ErrorState $ "Array `" ++ show array ++ "` not found in environment"
    _ -> ErrorState $ "Invalid array identifier"

-- Record field assignment
cmd_semantics (Assign (RecordAccess recordExp fieldExp) rhs) env c = \store ->
  exp_semantics recordExp env (\recordVal store' ->
    case recordVal of
      RecordVal _ recordEnv ->
        exp_semantics fieldExp recordEnv (\fieldVal store'' ->
          case fieldVal of
            Location loc ->
              exp_semantics rhs env (\rhsVal store''' ->
                case rhsVal of
                  RValue v   -> c (upsertStore loc (SValue v) store''')
                  Location l -> c (upsertStore loc (store''' l) store''')
                  _          -> ErrorState "Invalid assignment value"
              ) store''
            _ -> ErrorState "Record field must evaluate to a location"
        ) store'
      _ -> ErrorState "Not a record"
  ) store

-- (C2) Output:
-- C[output E] r c = R[E] r λe s. (e, s)
cmd_semantics (Output exp) env c = \store ->
  -- trace "Store before output :: " $ trace (show store) $
  exp_semantics exp env (\val store' ->
    case val of
      -- Create a new Result with the value and pass the continuation
      RValue v -> Result [v] (c store')
      Location _ -> Result [deref val store'] (c store')
      v@(ArrayVal _ _ _) -> Result [formatArrayOutput store' v] (c store')
      v@(RecordVal _ _)  -> Result [formatRecordOutput store' v] (c store')
      _ -> ErrorState "Output expression must evaluate to an R-value"
  ) store

-- (C3) Procedure Call:
-- C[E1(E2)] r c = E[E1] r ; Proc? λp . E[E2] r ; p ; c
cmd_semantics (CallProc procName args) env c = \store ->
  case env procName of
    ProcValue proc ->
      evalArgs args env store [] (\vals store' ->
        proc env c vals store')
    _ -> ErrorState $ "Undefined or invalid procedure: " ++ procName

-- (C4) Conditional:
-- C[if E then C1 else C2] r c = R[E] r ; Bool? ; cond(C[C1] r c, C[C2] r c)
cmd_semantics (IfCmd condition thenCmd elseCmd) env c = \store ->
  exp_semantics condition env (\conditionVal store' ->
    case conditionVal of
      RValue (Boolean True)  -> cmd_semantics thenCmd env c store'
      RValue (Boolean False) -> cmd_semantics elseCmd env c store'
      _ -> ErrorState $ "Expected a boolean in if condition, got: "
                      ++ show conditionVal
  ) store

-- (C5) While Loop:
-- C[while E do C] r c = R[E] r ; Bool? ; cond(C[C] r (C[while E do C] r c), c)
cmd_semantics (WhileDo condition body) env c = \store ->
  exp_semantics condition env (\conditionVal store' ->
    case conditionVal of
      RValue (Boolean True) -> cmd_semantics body env (\store'' ->
        cmd_semantics (WhileDo condition body) env c store'') store'
      RValue (Boolean False) -> c store'
      _ -> ErrorState $ "Expected a boolean in while condition, got: "
                      ++ show conditionVal
  ) store

-- (C6) Begin End Block:
-- C[begin D;C end] r c = D[D] r λr'.C[C] r[r'] c
cmd_semantics (BeginEnd decs cmds) env c = \store ->
  dec_semantics decs env (\extendedEnv store' ->
    cmd_semantics cmds extendedEnv c store'
  ) store

-- (C7) Command Block Sequence:
-- C[C1;C2] r c = C[C1] r ; C[C2] r ; c
cmd_semantics (CmdBlk c1 c2) env c = \store ->
  cmd_semantics c1 env (\store' ->
    cmd_semantics c2 env c store'
  ) store

-- Trap semantics:
-- C[trap C l1:C1,...,ln:Cn end] r c = C[C] r(C[C1] r c/l1,...,C[Cn] r c/ln) c
cmd_semantics (Trap body handlers) env c = \store ->
  -- First, create continuations for each handler
  let createLabelContinuation (label, handler) curEnv =
        -- Create a continuation that will execute the handler
        let handlerCont = \store' -> cmd_semantics handler env c store'
        -- Bind this continuation to the label in environment
        in upsertEnv label (LabelDef handlerCont) curEnv

      -- Extend environment with all handler continuations
      handlerEnv = foldr createLabelContinuation env handlers

  -- Now execute the body with the handler-extended environment
  in cmd_semantics body handlerEnv c store

-- Escapeto semantics:
-- C[escapeto l] r c = lookup(l,r); c
cmd_semantics (EscapeTo label) env c = \store ->
  case env label of
    LabelDef continuation -> continuation store  -- Invoke Cc if label found
    _ -> ErrorState $
         "No handler found for escape label: " ++ label

-- Declaration Semantics (D)
-- ----------------------------------------------------------------------------

-- D: Dec -> Env -> Dc -> Cc
dec_semantics :: Dec -> Env -> Dc -> Cc

-- (D1) Constant declaration:
-- D[const I = E] r u = R[E] r λe . u(e/I)
dec_semantics (Constant ide exp) env u = \store ->
  exp_semantics exp env (\val store' ->
    case val of
      RValue v -> u (upsertEnv ide (RValue v) env) store'
      Location _ -> u (upsertEnv ide (RValue (deref val store')) env) store'
      _ -> ErrorState $ "Constant declaration must evaluate to an R-value " ++ ide
  ) store

-- (D2) Variable declaration:
-- D[var I = E] r u = R[E] r; ref λi . u(i/I)
dec_semantics (Variable ide exp) env u = \store ->
  exp_semantics exp env (\val store' ->
    case val of
      RValue v ->
        let newLocation = allocate store'
            store''     = upsertStore newLocation (SValue v) store'
        in u (upsertEnv ide (Location newLocation) env) store''
      Location _ -> -- allocate a new location and copy the value
        let newLocation = allocate store'
            store''     = upsertStore newLocation (SValue (deref val store')) store'
        in u (upsertEnv ide (Location newLocation) env) store''
      _ -> ErrorState $ "Variable declaration must evaluate to an R-value" ++ ide
  ) store

-- (D3) Procedure declaration:
-- D[proc I(I1); C] r u = u(p/I) where p = (λc e . C[C] r[e/I1] c)
dec_semantics (Procedure name params body) declEnv u = \store ->
  let proc = \callEnv -> \c -> \args -> \store' ->
        -- Create environment for procedure execution using dynamic binding
        let preferredEnv = dynamicMix declEnv callEnv
            (procEnv, procStore) = bindArgs (zip params args) store' preferredEnv
        in cmd_semantics body procEnv c procStore
  in u (upsertEnv name (ProcValue proc) declEnv) store

-- Recursive Procedure
dec_semantics (RecProcedure name params body) declEnv u = \store ->
  let proc = \callEnv -> \c -> \args -> \store' ->
        -- Create environment with procedure bound to itself
        let recursiveEnv = upsertEnv name (ProcValue proc) declEnv
            -- Mix recursive environment with call environment
            preferredEnv = dynamicMix recursiveEnv callEnv
            -- Bind parameters in the recursive environment
            (procEnv, procStore) = bindArgs (zip params args) store' preferredEnv
        in cmd_semantics body procEnv c procStore
  -- Update environment with the recursive procedure
  in u (upsertEnv name (ProcValue proc) declEnv) store

-- (D4) Function declaration:
-- D[fun I(I1); E] r u = u(f/I) where f = (λk e . E[E] r[e/I] k)/I)
dec_semantics (Function name params body) declEnv u = \store ->
  let fun = \callEnv -> \k -> \args -> \store' ->
        -- Create environment for function execution using dynamic binding
        let preferredEnv = dynamicMix declEnv callEnv
            (funEnv, funStore) = bindArgs (zip params args) store' preferredEnv
        in exp_semantics body funEnv k funStore
  in u (upsertEnv name (FunValue fun) declEnv) store

-- Recursive Function
-- D[fun I(I1); E] r u = u(f/I) where rec f = (λk e . E[E] r[f,e/I,I1] k)/I)
dec_semantics (RecFunction name params body) declEnv u = \store ->
  let fun = \callEnv -> \k -> \args -> \store' ->
        -- Create environment with function bound to itself
        let recursiveEnv = upsertEnv name (FunValue fun) declEnv
            -- Mix recursive environment with call environment
            preferredEnv = dynamicMix recursiveEnv callEnv
            -- Bind parameters in the recursive environment
            (funEnv, funStore) = bindArgs (zip params args) store' preferredEnv
        in exp_semantics body funEnv k funStore
  -- Update environment with the recursive function
  in u (upsertEnv name (FunValue fun) declEnv) store

-- (D5) Sequence of declarations:
-- D[D1;D2] r u = D[D1] r λr1 . D[D2] r[r1] λr2 . u(r1[r2])
dec_semantics (DecBlk d1 d2) env u = \store ->
  dec_semantics d1 env (\extendedEnv store' ->
    dec_semantics d2 extendedEnv u store'
  ) store

-- Semantic function for array declaration
-- D[array I[E1;E2]] r u = E[E1] r \n1.E[E2] r \n2. (news n1,n2) k
dec_semantics (Array ide exp1 exp2) env u = \store ->
  -- Evaluate lower bound
  exp_semantics exp1 env (\val1 store' ->
    -- Evaluate upper bound
    exp_semantics exp2 env (\val2 store'' ->
      case (val1, val2) of
        (RValue (Numeric lb), RValue (Numeric ub)) ->
          newarray (lb, ub) (\arrayVal ->
            \s -> u (upsertEnv ide arrayVal env) s) store''
        _ -> ErrorState "Array bounds must be numeric"
    ) store'
  ) store

-- Semantic record for array declaration
-- D[record I[I1,...,In]] r u s
-- = (news n s = (l1,...ln,s')) → u(l1,...,ln/I1,...,In)/I) s',error
dec_semantics (Record name fields) env u = \store ->
  -- Call news to allocate n locations for n fields
  case news (fromIntegral (length fields)) store of
    Left err -> ErrorState err
    Right (locs, store') ->
      -- Create environment for record fields
      let recordEnv = \k -> case lookup k (zip fields locs) of
                             Just loc -> Location loc
                             Nothing -> Unbound
          -- Add record to environment and continue
          extendedEnv = upsertEnv name (RecordVal fields recordEnv) env
      in u extendedEnv store'


-- Run Interpreter
-- ----------------------------------------------------------------------------

run :: String -> [Value] -> Ans
run program input =
    unsafePerformIO $ catch
      (evaluate $ case sparse program of
        ParseOk (Program cmd) ->
          cmd_semantics cmd initEnv (\store -> Stop store) (initStore input)
        ParseError msg -> ErrorState ("Parser Error: " ++ msg))
      (\(e :: SomeException) ->    -- Show only the first line
         return $ ErrorState $ takeWhile (/= '\n') $ show e)

instance Show Ans where
    show (Stop store) = "Stop"
    show (ErrorState msg) = "ErrorState: " ++ msg
    show ans = "Result: " ++ show (flattenResults ans)

flattenResults :: Ans -> [Value]
flattenResults (Result values nextAns) = values ++ flattenResults nextAns
flattenResults (ErrorState msg) = [Str ("Error: " ++ msg)] -- Add err As Value
flattenResults (Stop _) = []        -- Stop accumulating at Stop

-- Debug helpers
instance Show EnvVal where
  show (Location loc) = "Location " ++ show loc
  show (RValue val) = "RValue " ++ show val
  -- show (ProcDef params _ _) = "ProcDef with params " ++ show params
  -- show (FunDef params _ _) = "FunDef with params " ++ show params
  show (ArrayVal lb ub locs) = "ArrayVal [" ++ show lb ++ ".." ++ show ub ++ "]"
  show (RecordVal fields _) = "RecordVal " ++ show fields
  show Unbound = "Unbound"

instance Show Store where
  show store = "Store: " ++ show [(i, store i) | i <- [0..10]]

instance Show StoreVal where
  show (File values) = "File: " ++ show values
  show (SValue value)  = "SValue: " ++ show value
  show ReservedArr = "ReservedArr"
  show Unused        = "Unused"

-- Helper Functions
-- ----------------------------------------------------------------------------

-- Helper to get parameter name from Args
getParamName :: Args -> Ide
getParamName (ValueParam name) = name
getParamName (ReferenceParam name) = name

-- Helper function to dereference an EnvVal to get its Value
deref :: EnvVal -> Store -> Value
deref envVal store = case envVal of
  Location loc -> case store loc of
                   SValue v -> v
                   ReservedArr -> Error "Unassigned"
                   _ -> error $ "Invalid location " ++ show loc
  RValue v -> v

-- Helper function to find the next available location [FIXME :: Store NextLoc]
allocate :: Store -> Integer
allocate store =
  case find (\i -> store i == Unused) [1..] of  -- Start from 1 since 0 is for input
    Just loc -> max loc (maxLocation store + 1)  -- Always use a new location
    Nothing -> error "No unused memory locations available"
  where
    maxLocation s = maximum [i | i <- [0..100], s i /= Unused]  -- Find highest used location

-- Helper function to update/insert the store
upsertStore :: Integer -> StoreVal -> Store -> Store
upsertStore loc val store = \key -> if key == loc then val else store key

-- Helper function to update/insert the env
upsertEnv :: Ide -> EnvVal -> Env -> Env
upsertEnv ide val env = \key -> if key == ide then val else env key

-- Helper function to display environment
showEnv :: [Ide] -> Env -> String
showEnv keys env = unlines [key ++ " -> " ++ show (env key) | key <- keys]

-- Helper function to evaluate Function/Procedure arguments
evalArgs :: [Exp] -> Env -> Store -> [EnvVal] -> ([EnvVal] -> Store -> Ans) -> Ans
evalArgs [] _ store evaluatedArgs k = k (reverse evaluatedArgs) store
evalArgs (arg:rest) env store evaluatedArgs k =
  exp_semantics arg env (\val store' ->
    evalArgs rest env store' (val:evaluatedArgs) k) store

-- Helper function to bind arguments to parameters
-- (call-by-value and call-by-reference)
bindArgs :: [(Args, EnvVal)] -> Store -> Env -> (Env, Store)
bindArgs paramArgs store env =
  foldr (\(param, argVal) (env', store'') ->
    case param of
      -- For var parameters: verify and bind location directly
      ReferenceParam name ->
        case argVal of
          Location loc -> (upsertEnv name (Location loc) env', store'')
          _ -> error $ "Pass by reference argument must be a Location, got "
                    ++ show argVal
      -- For value parameters: deref and create new location
      ValueParam name ->
        let value = case argVal of
              Location loc -> deref (Location loc) store''
              RValue v -> v
              _ -> error "Invalid value parameter"
            newLoc = allocate store''
            newStore = upsertStore newLoc (SValue value) store''
        in (upsertEnv name (Location newLoc) env', newStore)
  ) (env, store) paramArgs

-- Helper to check if a value is between bounds (inclusive)
between :: Integer -> Integer -> Integer -> Bool
between n1 n2 e = n1 <= e && e <= n2

-- Array Helper Functions
-- ----------------------------------------------------------------------------
-- news: Num -> Store -> ([Loc] × Store) + {error}
news :: Integer -> Store -> Either String ([Integer], Store)
news n store
  | n <= 0 = Left "Number of locations must be positive"
  | otherwise =
      -- Find first unused location
      let firstLoc = allocate store
          locs = [firstLoc..(firstLoc + n - 1)]
          newStore = foldr (\loc s -> upsertStore loc ReservedArr s) store locs
      in Right (locs, newStore)

-- newarray: [Num × Num] -> Ec -> Cc
newarray :: (Integer, Integer) -> Ec -> Cc
newarray (n1, n2) k = \store ->
  if n1 > n2
  then ErrorState "Lower bound greater than upper bound"
  else case news (n2 - n1 + 1) store of
         Left err -> ErrorState err
         Right (locs, store') -> k (ArrayVal n1 n2 locs) store'

-- subscript: Array -> Ec -> Ec
subscript :: EnvVal -> Ec -> Ec
subscript (ArrayVal lb ub locs) k = \val store ->
  case val of
    RValue (Numeric idx) ->
      if between lb ub idx
      then k (Location (locs !! fromIntegral (idx - lb))) store
      else ErrorState $ "Index " ++ show idx ++ " out of bounds ["
                       ++ show lb ++ ".." ++ show ub ++ "]"
    _ -> ErrorState "Array index must be numeric"
subscript _ _ = \_ _ -> ErrorState "Not an array"

-- Array Output Formatter
formatArrayOutput :: Store -> EnvVal -> Value
formatArrayOutput store (ArrayVal lb ub locs) =
  let elements = map (\(idx, loc) ->
          case store loc of
            SValue v    -> (show idx, v)
            ReservedArr -> (show idx, Error "Unassigned")
            _           -> (show idx, Error "Invalid Value")
        ) (zip [lb..ub] locs)
      formattedString = formatKeyValuePairs elements
  in Str formattedString
formatArrayOutput _ _ = Str "Invalid array value"

-- Record Output Formatter
formatRecordOutput :: Store -> EnvVal -> Value
formatRecordOutput store (RecordVal fields recordEnv) =
  let fieldValues = map (\field ->
          case recordEnv field of
            Location loc ->
              case store loc of
                SValue v -> (field, v)
                _        -> (field, Error "Unassigned")
            _ -> (field, Error "Invalid field")
        ) fields
      formattedString = formatKeyValuePairs fieldValues
  in Str formattedString
formatRecordOutput _ _ = Str "Invalid record value"

-- Format kv output
formatKeyValuePairs :: [(String, Value)] -> String
formatKeyValuePairs kvs =
  "{" ++ intercalate ", " (map (\(f, v) -> f ++ ": " ++ show v) kvs) ++ "}"
