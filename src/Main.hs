module Main where

import System.Environment ( getArgs )
import Data.Map.Lazy as LazyMap ()
import System.Exit ( exitFailure, exitSuccess )
import Latte.Par ( pProgram, myLexer )
import System.IO ( stderr, hPutStrLn, putStrLn )
import Frontend.Run 
import Frontend.Environment


printUsage :: IO ()
printUsage = putStrLn "usage: ./compiler <filename>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printUsage
    (_:_:_) -> printUsage
    [inputFile] -> do
      code <- readFile inputFile
      case pProgram $ myLexer code of
        Left errMessage -> do
          hPutStrLn stderr $ "ERROR\n" ++ errMessage
          exitFailure
        Right tree -> do
          (output, _) <- runSemanticAnalysis tree
          case output of
            Left message -> hPutStrLn stderr message
            Right _ -> do
                hPutStrLn stderr "OK\n"
                exitSuccess
