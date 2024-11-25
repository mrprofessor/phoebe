{-# LANGUAGE BlockArguments #-}

module Parser where
import Parsing

-- Identifier
type Ide = String

 -- Basic Constants
data BasicConstant
  = Number Integer                         -- Numeric constants ( 0, 1, 42)
  | Bool Bool                              -- Boolean constants (true, false)
  deriving (Eq, Show)

-- Operators
data ArithmeticOp = Plus | Minus | Times | Div
  deriving (Eq, Show)

data RelationalOp = Equal | Greater | Lesser
  deriving (Eq, Show)

data BinaryOp = ArithmeticOp ArithmeticOp | RelationalOp RelationalOp
  deriving (Eq, Show)

data UnaryOp = Not
  deriving (Eq, Show)

-- Expressions
data Exp
  = BasicConstant BasicConstant            -- Basic constants(Number, Boolean)
  | Read                                   -- The read expression (from input)
  | I Ide                                  -- Identifiers
  | UnaryOp UnaryOp Exp                    -- Unary operators(Not)
  | BinaryOp BinaryOp Exp Exp              -- Binary operators
  | Call Ide Exp                           -- Function calls
  | IfThenElseExp Exp Exp Exp              -- "if E then E1 else E2"
  deriving (Eq, Show)

-- Commands
data Cmd
  = Skip                                   -- The skip command
  | Output Exp                             -- Output E
  | Assign Ide Exp                         -- Assignment "I := E"
  | IfThenElseCmd Exp Cmd Cmd              -- "if E then C1 else C2"
  | WhileDo Exp Cmd                        -- "while E do C"
  | Seq Cmd Cmd                            -- Sequence of commands "C1; C2"
  deriving (Eq, Show)

-- Declarations
data Dec
  = Const Ide Exp                          -- Constant "const I = E"
  | Var Ide Exp                            -- Variable "var I = E"
  | Fun Ide [Ide] Cmd                      -- Function "fun I (I1, ..., In) C"
  | Proc Ide [Ide] Cmd                     -- Procedure "proc I (I1, ..., In) C

-- Statements
data Stmt
  = Exp Exp                                -- Expression
  | Cmd Cmd                                -- Command
  deriving (Eq, Show)

-- Statement Sequences
data StmtSeq
  = SingleStmt Stmt                        -- Single statement
  | MultipleStmt Stmt StmtSeq                   -- Sequence of statements
  deriving (Eq, Show)


-- GRAMMAR: <expr> ::= <term> + <expr> | <term> = <expr> | <term>
expr :: Parser Exp
expr = 
     do e1 <- term
        symbol "+"
        e2 <- expr
        return (BinaryOp (ArithmeticOp Plus) e1 e2)
      +++
      do e1 <- term
         symbol "-"
         e2 <- expr
         return (BinaryOp (ArithmeticOp Minus) e1 e2)
      +++
      do e1 <- term
         symbol "*"
         e2 <- expr
         return (BinaryOp (ArithmeticOp Times) e1 e2)
      +++
      do e1 <- term
         symbol "/"
         e2 <- expr
         return (BinaryOp (ArithmeticOp Div) e1 e2)
      +++
      do e1 <- term
         symbol "="
         e2 <- expr
         return (BinaryOp (RelationalOp Equal) e1 e2)
      +++
      do e1 <- term
         symbol ">"
         e2 <- expr
         return (BinaryOp (RelationalOp Greater) e1 e2)
      +++
      do e1 <- term
         symbol "<"
         e2 <- expr
         return (BinaryOp (RelationalOp Lesser) e1 e2)
      +++
      do symbol "if"
         cond <- expr
         symbol "then"
         e1 <- expr
         symbol "else"
         e2 <- expr
         return (IfThenElseExp cond e1 e2)
      +++
      term

-- GRAMMAR: <term> ::= not <expr> | <factor>
term :: Parser Exp
term = do symbol "not"
          e <- expr
          return (UnaryOp Not e)
        +++
          factor

-- GRAMMAR: <factor> ::= 0 | 1 | TT | FF | <ide> | <expr>
factor :: Parser Exp
factor = 
        do n <- nat
           return (BasicConstant (Number (toInteger n)))
        +++
        do symbol "true"
           return (BasicConstant (Bool True))
        +++
        do symbol "false"
           return (BasicConstant (Bool False))
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
       return (IfThenElseCmd e c1 c2)
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
cmdseq = do
  c1 <- cmd
  (do symbol ";"
      c2 <- cmdseq
      return (Seq c1 c2)
   +++
   do symbol ";"                           -- Optional trailing semicolon
      return c1
   +++
   return c1)                              -- No semicolon
  
-- Parse a block of commands enclosed in curly braces
-- Grammar: <cmdblock> ::= { <cmdseq> }
cmdblock :: Parser Cmd
cmdblock = do symbol "{"
              c <- cmdseq
              symbol "}"
              return c
           +++ cmd

-- Parse Statements (Statements are either commands or expressions)
-- Grammar: <Cmd> | <Exp>
stmt :: Parser Stmt
stmt =
  do cmd <- cmd
     return (Cmd cmd)
  +++
  do exp <- expr
     return (Exp exp)

-- Parse Statement Sequences
stmtSeq = do 
  s1 <- stmt
  (do symbol ";"
      rest <- stmtSeq
      return (MultipleStmt s1 rest)
   +++
   do symbol ";"
      return (SingleStmt s1)
   +++
   return (SingleStmt s1))

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

-- Parse Statements
sparse :: String -> Maybe StmtSeq
sparse xs = case (parse stmtSeq xs) of
              [(n, [])]  -> Just n
              [(_, out)] -> Nothing
              []         -> Nothing
  
