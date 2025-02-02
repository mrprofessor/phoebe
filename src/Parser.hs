module Parser where
import Parsing
import Debug.Trace (trace) -- For debugging

-------------------------------------------------------------------------------
-- PARSER
-------------------------------------------------------------------------------

type Ide = String

-- P ::= program C
data Program = Program Cmd
    deriving (Show, Eq)

-- E :: = B | true | false | read | I | E1 (E2) | if E then E1 else E2 | E O E2
data Exp
  = Number Integer
  | Bool Bool
  | String String
  | Read
  | Identifier Ide
  | CallFun Ide [Exp]                 -- funcName(args)
  | IfExp Exp Exp Exp
  | BinOp String Exp Exp
  | ArrayAccess Exp Exp               -- Array access using index A[i]
  | RecordAccess Exp Exp              -- Record access using field R.f or R[I]
  deriving (Show, Eq)

-- C::= E1 := E2 | output E | E1(E2) | if E then C1 else C2 | while E do C | begin D;C end | C1 ;C2
data Cmd
  = Assign Exp Exp
  | Output Exp
  | CallProc Ide [Exp]                -- Later support CallProc Exp [Exp]
  | IfCmd Exp Cmd Cmd
  | WhileDo Exp Cmd
  | BeginEnd Dec Cmd
  | CmdBlk Cmd Cmd
  | Trap Cmd [(Ide, Cmd)]             -- trap C [I1: C1, I2: C2, ...] end
  | EscapeTo Ide
  | Label Ide Cmd
  deriving (Show, Eq)

-- D ::= const I = E | var I = E | proc I(I1),C | fun I(I1),E | D1;D2
data Dec
  = Constant Ide Exp
  | Variable Ide Exp
  | Procedure Ide [Args] Cmd          -- Regular procedure
  | RecProcedure Ide [Args] Cmd       -- Recursive procedure
  | Function Ide [Args] Exp           -- Regular function
  | RecFunction Ide [Args] Exp        -- Recursive function
  | Array Ide Exp Exp                 -- Array I[E1;E2]
  | Record Ide [Ide]                  -- Record I[I1, I2, ...]
  | DecBlk Dec Dec
  deriving (Show, Eq)

data ParsedResult
  = ParseOk Program
  | ParseError String
  deriving (Show, Eq)

data Args
  = ValueParam Ide                    -- Pass-by-value
  | ReferenceParam Ide                -- Pass-by-reference
  deriving (Show, Eq)

binop :: Parser String
binop = do   symbol "<="  ; return "<="
      +++ do symbol ">="  ; return ">="
      +++ do symbol "=="  ; return "=="
      +++ do symbol "+"   ; return "+"
      +++ do symbol "-"   ; return "-"
      +++ do symbol "*"   ; return "*"
      +++ do symbol "/"   ; return "/"
      +++ do symbol "%"   ; return "%"
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
     thenCmd <- expr
     symbol "else"
     elseCmd <- expr
     return (IfExp cond thenCmd elseCmd)
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
  do symbol "\""
     s <- many (sat (/= '\"'))
     symbol "\""
     return (String s)
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
     (do symbol "["                    -- Array access parsing
         idx <- expr                   -- Array index
         symbol "]"
         return (ArrayAccess (Identifier id) idx)
      +++
      do symbol "."                    -- Simple record field access
         field <- expr                 -- FIXME Supposed to be an identifier
         return (RecordAccess (Identifier id) field)
      +++                             -- If no [ or . follows, it's just an ide
         return (Identifier id))
  +++
  do symbol "("
     e <- expr
     symbol ")"
     return e

cmd :: Parser Cmd
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
     decs <- decBlk
     symbol ";"                       -- Mandatory ; after declarations
     cmds <- cmdBlk
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
     thenCmd <- cmd
     symbol "else"
     elseCmd <- cmd
     return (IfCmd cond thenCmd elseCmd)
  +++
  do name <- token identifier
     symbol "("
     args <- expr `sepby` (symbol ",")
     symbol ")"
     return (CallProc name args)
  +++
  do symbol "{"
     c <- cmdBlk
     symbol "}"
     return c
  +++
  do symbol "trap"
     body <- cmdBlk
     labels <- sepby (do
       l <- token identifier
       symbol ":"
       c <- cmd
       return (l, c)) (symbol ",")
     symbol "end"
     return (Trap body labels)
  +++

  do symbol "escapeto"
     label <- token identifier
     return (EscapeTo label)
  +++
  do name <- token identifier
     symbol ":"
     rest <- cmd
     return (Label name rest)

-- Sequence of commands
cmdBlk :: Parser Cmd
cmdBlk =
  do c1 <- cmd
     (do symbol ";"
         rest <- cmdBlk
         return (CmdBlk c1 rest)
      -- +++                          -- Make ; optional for the last command
      -- do symbol ";"
      --    return c1)
      +++
      return c1)

-- Declaration parsers
dec :: Parser Dec
dec =
  do symbol "record"
     name <- identifier
     symbol "("
     fields <- identifier `sepby` (symbol ",")
     symbol ")"
     return (Record name fields)
  +++
  do symbol "array"                   -- Parse array declaration
     id <- token identifier           -- Array name
     symbol "["
     e1 <- expr                       -- Lower bound
     symbol ":"
     e2 <- expr                       -- Upper bound
     symbol "]"
     return (Array id e1 e2)
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
     params <- parameters `sepby` (symbol ",")
     symbol ")"
     symbol "->"
     body <- cmd
     return (Procedure name params body)
  +++
  do symbol "fun"
     name <- token identifier
     symbol "("
     params <- parameters `sepby` (symbol ",")
     symbol ")"
     symbol "->"
     body <- expr                     -- Functions return exps, not cmds
     return (Function name params body)
  +++
  do symbol "rec"                     -- Recursive procedure
     symbol "proc"
     name <- token identifier
     symbol "("
     params <- parameters `sepby` (symbol ",")
     symbol ")"
     symbol "->"
     body <- cmd
     return (RecProcedure name params body)
  +++
  do symbol "rec"                     -- Recursive function
     symbol "fun"
     name <- token identifier
     symbol "("
     params <- parameters `sepby` (symbol ",")
     symbol ")"
     symbol "->"
     body <- expr
     return (RecFunction name params body)

parameters :: Parser Args
parameters =
  do symbol "var"
     paramName <- identifier
     return (ReferenceParam paramName)
  +++
  do paramName <- identifier
     return (ValueParam paramName)

-- Sequence of Declarations
decBlk :: Parser Dec
decBlk =
  do d1 <- dec
     (do symbol ";"
         rest <- decBlk
         return (DecBlk d1 rest)
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
             [(result, out_)] -> ParseError ("Unused Syntax " ++ out_)
             []               -> ParseError "Invalid Syntax"
