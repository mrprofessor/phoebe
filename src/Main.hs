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
