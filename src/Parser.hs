module Parser where
import Parsing


-- Type and Data declarations
type Ide = String

data Exp = Number Integer
  | Bool Bool
  | Read
  | I Ide
  | Not Exp
  | Equal Exp Exp
  | Greater Exp Exp
  | Lesser Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  deriving (Eq, Show)

data Cmd = Skip
  | Output Exp
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
       do e1 <- term
          symbol ">"
          e2 <- expr
          return (Greater e1 e2)
        +++
       do e1 <- term
          symbol "<"
          e2 <- expr
          return (Lesser e1 e2)
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
             return (Number (toInteger n))) -- 0 | 1 | 2 ....
         +++
         (do symbol "true"
             return (Bool True))  -- TT
         +++
         (do symbol "false"
             return (Bool False)) -- FF
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
-- Grammar: <cmd> :: skip
--                   | output <expr>
--                   | if <expr> then <cmdblock> else <cmdblock>
--                   | while <expr> do <cmdblock>
--                   | <ide> := <expr>
cmd :: Parser Cmd
cmd = 
    do symbol "skip"
       return Skip
    +++
    do symbol "output"
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
eparse :: String -> Maybe Exp
eparse xs = case (parse expr xs) of
              [(n, [])]  -> Just n
              [(_, out)] -> Nothing
              []         -> Nothing

-- Parse Commands
cparse :: String -> Maybe Cmd
cparse xs = case (parse cmdseq xs) of
              [(n, [])]  -> Just n
              [(_, out)] -> Nothing
              []         -> Nothing

