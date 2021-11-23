module Main where

--import Compiler ( compile )
import System.Environment ( getArgs )
import Data.Map.Lazy as LazyMap ()
import System.Exit ( exitFailure, exitSuccess )
import Grammar.Test ( printTest )

main :: IO ()
main = do
  print printTest;