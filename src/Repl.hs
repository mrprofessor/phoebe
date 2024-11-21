module Repl where
import System.IO (hFlush, stdout)

import Parser
import Interpreter

-- Helper function to retrieve the output from the State
getOutput :: State -> [Value]
getOutput (_, _, o) = o

-- Read, Eval, Print Loop

read_ :: IO String
read_ = putStr "(^._.^) >>"
        >> hFlush stdout
        >> getLine

eval_ :: String -> State -> (String, State)
eval_ input state =
  case sparse input of
    Just stmt ->
      case stmt_semantics stmt state of
        OKc newState -> ("Output: " ++ show (getOutput newState), newState)
        Errorc msg -> ("Command Evaluation Failed. \n" ++ msg, state)
    Nothing -> ("Invalid Input. Please enter a valid command or expression.", state)


print_ :: String -> IO ()
print_ = putStrLn

loop_ :: State -> IO ()
loop_ state = do
  input <- read_
  case input of
    ":quit" -> putStrLn "Goodbye!"
    ":q" -> putStrLn "Goodbye!"
    _ -> do
      let (output, newstate) = eval_ input state
      print_ output
      loop_ newstate

main :: IO ()
main = do
  putStrLn "Welcome to the Phoebe REPL"
  putStrLn "Type :quit or :q to exit"
  loop_ (emptymem, [], [])
