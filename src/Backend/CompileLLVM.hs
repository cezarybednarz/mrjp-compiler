{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Backend.CompileLLVM where

import Control.Monad.State ( MonadState(put, get) )
import Control.Monad.Reader ( MonadReader(ask, local) )
import Control.Monad.Except ()

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

-- compilation --
compileTopDefs :: [TopDef] -> CM [()]
compileTopDefs = mapM compileTopDef

compileTopDef :: TopDef -> CM ()
compileTopDef (FnDef line t id args b) = do
  -- todo args declaration
  return ()


-- compile statements --
compileBlock :: Stmt -> CM (RetInfo, Env)
compileBlock block = do
  (valEnv, funcEnv, fnRetReg, scope) <- ask
  case block of
    BStmt line (Block line2 b) -> do
      local (const (valEnv, funcEnv, fnRetReg, scope+1)) $ compileBlockStmts b
    stmt ->
      local (const (valEnv, funcEnv, fnRetReg, scope+1)) $ compileBlockStmts [stmt]

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