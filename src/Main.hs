module Main where

import System.Environment (getArgs)
import Interpreter

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      contents <- readFile fileName
      let results = run contents []
      putStrLn $ show results
    _ -> putStrLn "Usage: interpreter <filename>"


-- cmd_semantics (ForLoop var start end body) env c = \store ->
--   -- First evaluate start expression
--   exp_semantics start env (\startVal store1 ->
--     -- Then evaluate end expression
--     exp_semantics end env (\endVal store2 ->
--       case (startVal, endVal) of
--         (RValue (Numeric start'), RValue (Numeric end')) ->
--           -- Create new environment with loop variable
--           let newLoc = allocate store2
--               initStore = upsertStore newLoc (SValue (Numeric start')) store2
--               loopEnv = upsertEnv var (Location newLoc) env

--               -- Helper function to execute one iteration
--               iter currentVal st =
--                 if currentVal <= end'
--                 then
--                   -- Execute loop body
--                   cmd_semantics body loopEnv (\store' ->
--                     -- Increment counter and continue
--                     let nextVal = currentVal + 1
--                         store'' = upsertStore newLoc (SValue (Numeric nextVal)) store'
--                     in iter nextVal store''
--                   ) st
--                 else
--                   -- Loop finished, continue with rest of program
--                   c st
--           in iter start' initStore

--         _ -> ErrorState "For loop bounds must be numeric values"
--     ) store1
--   ) store
--
--

-- -- For Loop semantics:
-- -- C[for I: = F do C] r c = E[I] r;Loc? λl . F[F] r (λc' . update l;C[C] r c') c
-- cmd_semantics (ForLoop i e1 e2 body) env c = \store ->
--   case env i of
--     Location l ->
--     -- Create procedure p = λc'.update l;C[C] r c'
--       let p = \val c' store' ->
--             case val of
--               RValue v ->
--                 let store'' = upsertStore l (SValue v) store'
--                 in cmd_semantics body env c' store''
--               _ -> ErrorState "For loop value must be R-value"
--           -- Convert e1 to e2 into step/until form
--           forlist = ForStepE e1 (Number 1) e2
--       in for_semantics forlist env p c store
--     _ -> ErrorState "For loop variable must be a location"


-- -- ForList Semantics (F)
-- -------------------------------------------------------------------------------

-- -- ForList Domain
-- -- F :: = E | E1 while E2 | E1 step E2 until E3 | F1 ,F2
-- data ForList = ForE Exp | ForWhileE Exp Exp | ForStepE Exp Exp Exp | ForSeq ForList ForList
--   deriving (Show, Eq)

-- -- ForList Semantics
-- -- F: For -> Env -> Proc -> Cc -> Cc
-- for_semantics :: ForList -> Env -> ProcDef -> Cc -> Cc

-- -- F[E] r p c = R[E] r p c
-- for_semantics (ForE e) env proc c = \store ->
--   exp_semantics e env proc c store

-- -- F[E1 while E2] r p c = R[E1] r λe . R[E2] r;Bool?cond(p(F[E1 while E2] r p c)e,c)
-- for_semantics (ForWhileE e1 e2) env proc c = \store ->
--   exp_semantics e1 env (\val1 store1 ->
--     exp_semantics e2 env (\val2 store2 ->
--       case val2 of
--         RValue (Boolean True) ->
--           proc val1 (\store3 ->
--             for_semantics (ForWhileE e1 e2) env proc c store3) store2
--         RValue (Boolean False) -> c store2
--         _ -> ErrorState "While condition must be boolean"
--     ) store1
--   ) store

-- -- F[E1 step E2 until E3] r p c = R[E1] r;Num?;step(R[E2] r,R[E3] r) p c
-- for_semantics (ForStepE e1 e2 e3) env proc c = \store ->
--   exp_semantics e1 env (\val1 store1 ->
--     case val1 of
--       RValue (Numeric n) ->
--         exp_semantics e2 env (\step_val store2 ->
--           exp_semantics e3 env (\until_val store3 ->
--             case (step_val, until_val) of
--               (RValue (Numeric step), RValue (Numeric until)) ->
--                 let sign_step = if step >= 0 then 1 else -1
--                     continue = (n - until) * sign_step < 1
--                 in if continue
--                    then
--                      let next_store = proc (RValue (Numeric n)) c store3
--                      in for_semantics (StepE (Number (n + step)) e2 e3) env proc c next_store
--                    else c store3
--               _ -> ErrorState "Step expressions must evaluate to numbers"
--           ) store2
--         ) store1
--       _ -> ErrorState "Initial value must be numeric"
--   ) store

-- -- F[F1,F2] r p c = F[F1] r p;F[F2] r p;c
-- for_semantics (ForSeq f1 f2) env proc c = \store ->
--   for_semantics f1 env proc (for_semantics f2 env proc c) store
