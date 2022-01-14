{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Backend.CompileLLVM where

import Control.Monad.State 
import Control.Monad.Reader
import Control.Monad.Except

import Backend.Environment
import Backend.LLVM as LLVM
import Latte.Abs



-- function for starting compilation --
runMain :: Program -> CM String
runMain (Program line tds) = do
  env <- compileTopDefs tds
  return "currently nothing"

-- env modifiers --


-- compiler state modifiers --

-- add function to compiler state --
emitFunction :: LLVM.Type -> String -> [(LLVM.Type, String)] -> CM ()
emitFunction t name args = do
  state <- get
  let functions = sFunctions state
  let function = Fn {
    fType = t,
    fName = name,
    fArgs = args,
    fBlocks = []
  }
  put $ state { sFunctions = function:functions}
  return ()

-- add empty block to current function -- 
emitNewBlock :: Label -> CM ()
emitNewBlock label = do
  state <- get 
  let f:functions = sFunctions state 
  let blocks = fBlocks f
  let block = LLBlock {
    bLabel = label,
    bStmts = []
  }
  let function = f { fBlocks = block:blocks }
  put $ state { sFunctions = function:functions }
  return ()

-- emit instruction to current block in current function -- 
emitStmt :: LLVMStmt -> CM ()
emitStmt llvmstmt = do
  state <- get 
  let f:functions = sFunctions state
  let b:blocks = fBlocks f
  let llvmstmts = bStmts b
  let block = b { bStmts = llvmstmt:llvmstmts }
  let function = f { fBlocks = block:blocks }
  put $ state { sFunctions = function:functions}
  return ()

-- compile topdefs --
compileTopDefs :: [TopDef] -> CM [()]
compileTopDefs = mapM compileTopDef

compileTopDef :: TopDef -> CM ()
compileTopDef (FnDef line t id args b) = do
  -- todo args declaration
  compileBlock (BStmt line b)
  return ()


-- compile statements --
compileBlock :: Stmt -> CM (RetInfo, Env)
compileBlock block = do
  env <- ask
  case block of
    BStmt line (Block line2 b) -> do
      local (const (env { eScope = eScope env + 1})) $ compileBlockStmts b
    stmt ->
      local (const (env { eScope = eScope env + 1})) $ compileBlockStmts [stmt]

compileBlockStmts :: [Stmt] -> CM (RetInfo, Env)
compileBlockStmts [] = do
  env <- ask
  return (ReturnNothing, env)
compileBlockStmts (s:ss) = do 
  ret <- compileStmt s
  case ret of
    (Return val, _) -> do
      case val of 
        (Tvoid, _) -> emitStmt RetVoid
        (t, v) -> emitStmt $ LLVM.Ret t v
      return ret
    (ReturnNothing, env) -> do
      local (const env) $ compileBlockStmts ss


compileStmt :: Stmt -> CM (RetInfo, Env)
compileStmt (Empty _) = do
  env <- ask
  return (ReturnNothing, env)
-- todo compileStmt wszystkiego