module Parser where
import Parsing


-- Type and Data declarations
type Ide = String

data Exp = Number Integer
  | TT
  | FF
  | Read
  | I Ide
  | Not Exp
  | Equal Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  deriving (Eq, Show)

data Cmd = Output Exp
  | Assign Ide Exp
  | IfThenElse Exp Cmd Cmd
  | WhileDo Exp Cmd
  | Seq Cmd Cmd
  deriving (Eq, Show)

-- GRAMMAR: <expr> ::= <term> + <expr> | <term> = <expr> | <term>
expr :: Parser Exp
expr = do e1 <- term
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
factor = (do n <- nat
             return (Number (toInteger n)))
         +++
         (do symbol "true"
             return TT)
         +++
         (do symbol "false"
             return FF)
         +++
         (do symbol "read"
             return Read)
         +++
         (do id <- token identifier
             return (I id))
         +++
         (do symbol "("
             e <- expr
             symbol ")"
             return e)

-- Parse Commands
-- Grammar: <cmd> :: <ide>:=<expr>
--                   | output
--                   | if <expr> then <cmd> else <cmd>
--                   | while <expr> do <cmd>
cmd :: Parser Cmd
cmd = do symbol "output"
         e <- expr
         return (Output e)
       +++
       do symbol "if"
          e <- expr
          symbol "then"
          c1 <- cmdblock
          symbol "else"
          c2 <- cmdblock
          return (IfThenElse e c1 c2)
       +++
       do symbol "while"
          e <- expr
          symbol "do"
          c <- cmdblock
          return (WhileDo e c)
       +++
       do id <- token identifier
          symbol ":="
          e <- expr
          return (Assign id e)

-- Parse sequences of commands separately to prevent infinite recursion
-- Grammar <cmdseq> :: <cmd> ; <cmd> | <cmd> | <cmd> ;
cmdseq :: Parser Cmd
cmdseq = do c1 <- cmd
            (do symbol ";"
                (do c2 <- cmdseq
                    return (Seq c1 c2)
                 +++
                 return c1)  -- Optional trailing semicolon
             +++
             return c1)      -- No semicolon
  

-- Parse a block of commands enclosed in curly braces
-- Grammar: <cmdblock> ::= { <cmdseq> }
cmdblock :: Parser Cmd
cmdblock = do symbol "{"
              c <- cmdseq
              symbol "}"
              return c
           +++ cmd
            

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

