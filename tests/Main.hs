module Main where

import Test.Hspec
import qualified TestParser
import qualified TestInterpreter

main :: IO ()
main = hspec $ do
  describe "Phoebe Language Tests" $ do
    describe "Parser Tests" TestParser.spec
    describe "Interpreter Tests" TestInterpreter.spec
