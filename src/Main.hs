module Main where

import           Data.Map.Lazy        as LazyMap ()
import           Frontend.Environment
import           Frontend.Run
import           Latte.Par            (myLexer, pProgram)
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure, exitSuccess)
import           System.IO            (hPutStrLn, putStrLn, stderr)
import Backend.Run
import Optimizations.Mem2Reg
import Optimizations.GCSE
import Backend.LLVM


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
            Left message -> do
              hPutStrLn stderr message
              exitFailure
            Right _ -> do
              hPutStrLn stderr "OK\n"
              (Right llvmProgram, _) <- runCompilation tree
              (llvmProgram, _) <- runMem2Reg llvmProgram
              (llvmProgram, _) <- runTrivialPhiReduction llvmProgram
              (llvmProgram, _) <- runGCSE llvmProgram
              -- todo runTrivialPhiReduction
              putStrLn $ unlines $ printLLVMProgram (pStrConstants llvmProgram) (pFunctions llvmProgram)
