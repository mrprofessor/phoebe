module AST where
import Parsing
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)

type Ide = String

-- P ::= program C
data Program = Program Dec Com
  deriving Show

-- E :: = B | true | false | read | I | E1 (E2) | if E then E1 else E2 | E O E2
data Exp
  = Number Integer
  | Bool Bool
  | Read
  | VarRef Ide
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
  | Procedure Ide [Ide] Com
  | Function Ide [Ide] Exp
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
     return (VarRef id)
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
     symbol ";"                -- Mandatory ; after declaration 
     c <- cmd
     symbol "end"
     return (BeginEnd decs c)
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
      +++
      do symbol ";"
         return c1
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

-- Sequence of Declarations
decSeq :: Parser Dec
decSeq =
  do d1 <- dec
     (do symbol ";"
         rest <- decSeq
         return (DecSeq d1 rest)
      +++
      do symbol ";"
         return d1
      +++
      return d1)

-- Program parser
program :: Parser Program
program =
  do dec <- decSeq
     cmd <- cmd
     return (Program dec cmd)

-- Main parse function
sparse :: String -> ParsedResult
sparse xs = case Parsing.parse program xs of
             [(result, [])]   -> ParseOk result
             [(result, out_)] -> ParseError ("Unused input " ++ out_)
             []               -> ParseError "Invalid input"
