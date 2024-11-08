module Main where

import qualified Interpreter (run)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let result = Interpreter.run "x := 1; output x" []
  print result
