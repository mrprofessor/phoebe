module Parser where
import Parsing


-- Type and Data declarations
type Ide = String

data Exp = Zero
  | One
  | Two
  | Three
  | TT
  | FF
  | Read
  | I Ide
  | Not Exp
  | Equal Exp Exp
  | Plus Exp Exp
  deriving Show

data Cmd = Assign Ide Exp
  | Output Exp
  | IfThenElse Exp Cmd Cmd
  | WhileDo Exp Cmd
  | Seq Cmd Cmd
  deriving Show

-- GRAMMAR: <expr> ::= <term> + <expr> | <term> = <expr> | <term>
expr :: Parser Exp
expr = do e1 <- term
          symbol "+"
          e2 <- expr
          return (Plus e1 e2)
        +++
       do e1 <- term
          symbol "="
          e2 <- expr
          return (Equal e1 e2)
        +++
          term

-- GRAMMAR: <term> ::= not <expr> | <factor>
term :: Parser Exp
term = do symbol "not"
          e <- expr
          return (Not e)
        +++
          factor

-- GRAMMAR: <factor> ::= 0 | 1 | TT | FF | <ide> | <expr>
factor :: Parser Exp
factor = do symbol "0"
            return Zero
          +++
          do symbol "1"
             return One
          +++
          do symbol "2"
             return Two
          +++
          do symbol "3"
             return Three
          +++
          do symbol "true"
             return TT
          +++
          do symbol "false"
             return FF
          +++
          do symbol "read"
             return Read
          +++
          do id <- token identifier
             return (I id)
          +++
          do symbol "("
             e <- expr
             symbol ")"
             return e

-- Parse Commands
-- Grammar: <cmd> :: <ide>:=<expr> | output | if <expr> then <cmd> else <cmd>
--                 | while <expr> do <cmd>
cmd :: Parser Cmd
cmd = do symbol "output"
         e <- expr
         return (Output e)
       +++
       do symbol "if"
          e <- expr
          symbol "then"
          c1 <- cmd
          symbol "else"
          c2 <- cmd
          return (IfThenElse e c1 c2)
       +++
       do symbol "while"
          e <- expr
          symbol "do"
          c <- cmd
          return (WhileDo e c)
       +++
       do id <- token identifier
          symbol ":="
          e <- expr
          return (Assign id e)

-- Parse sequences of commands separately to prevent infinite recursion
-- Grammar <cmdseq> :: <cmd> ; <cmd>
cmdseq :: Parser Cmd
cmdseq = do c1 <- cmd
            symbol ";"
            c2 <- cmdseq
            return (Seq c1 c2)
          +++
            cmd
            

-- Parse Expressions
eparse :: String -> Exp
eparse xs = case (parse expr xs) of
              [(n, [])]  -> n
              [(_, out)] -> error ("Unused input " ++ out)
              []         -> error "Invalid input"
              
-- Parse Commands
cparse :: String -> Cmd
cparse xs = case (parse cmdseq xs) of
              [(n, [])]  -> n
              [(_, out)] -> error ("Unused input " ++ out)
              []         -> error "Invalid input"

-- Test cases
-- Test 1: eparse "1+1"
-- Test 2: eparse "1=1"
-- Test 3: eparse "not 1"

-- Test 4: cparse "output 1"
-- Test 5: cparse "if 1 then output 1 else output 0"
-- Test 6: cparse "while 1 do output 0"
-- Test 7: cparse "x:=1; output x"
-- Gordon Test Cases
-- cparse "sum:=0; x:=read; while not (x=1) do sum:=sum+x; x:=read; output sum"

