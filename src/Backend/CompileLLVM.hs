{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Backend.CompileLLVM where

import Control.Monad.State 
import Control.Monad.Reader
import Control.Monad.Except

import Backend.Environment
import Backend.LLVM as LLVM
import Latte.Abs as Latte


-- function for starting compilation --
runMain :: Program -> CM String
runMain (Program line tds) = do
  env <- compileTopDefs tds
  return "currently nothing"

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

-- add function to compiler state --
emitFunction :: LLVM.Type -> String -> [(LLVM.Type, String)] -> CM ()
emitFunction t name args = do
  -- todo emit alloc
  -- todo modify env
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

-- env modifiers --
declItem :: Latte.Type -> Item -> CM Env
declItem t (NoInit line id) = do
  -- todo modify env
  -- todo add alloc
declItem t (Init line id e) = do
  -- todo modify anv
  -- todo emit alloc

-- convert between latte and llvm types --
convIdentString :: Ident -> CM String 
convIdentString (Ident id) = id

convTypeLLVMType :: Latte.Type -> CM Val
convTypeLLVMType t = do
  case t of
    Int _ ->
      return Ti32 
    Bool _ -> 
      return Ti1
    Str _ ->
      return Ptr Ti8
    Void _ ->
      return Tvoid

convArgExpr :: Arg -> CM Expr
convArgExpr (Arg _ t _) =
  case t of
    (Bool _) -> return $ ELitFalse Nothing
    (Int _)  -> return $ ELitInt Nothing 0
    (Str _)  -> return $ EString Nothing ""

convArgTofArg :: Arg -> CM (LLVM.Type, String)
convArgTofArg (Arg _ t id) = do
  ident <- convIdentString id
  llvmtype <- convTypeLLVMType t
  return (llvmtype, ident)

-- compile topdefs --
compileTopDefs :: [TopDef] -> CM [()]
compileTopDefs = mapM compileTopDef

compileTopDef :: TopDef -> CM ()
compileTopDef (FnDef line t id args b) = do
  fArgs <- mapM convArgTofArg args
  ident <- convIdentString id
  llvmtype <- convTypeLLVMType t
  env <- emitFunction llvmtype ident fArgs
  local (const env) $ compileBlock (BStmt line b)
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

compileDecl :: Latte.Type -> [Item] -> CM Env
compileDecl t [] = ask
compileDecl t (x:xs) = do
  env <- declItem t x
  local (const env) $ analyseDecl t xs

compileStmt :: Stmt -> CM (RetInfo, Env)
compileStmt (Empty _) = do
  env <- ask
  return (ReturnNothing, env)
compileStmt (BStmt line (Block line2 b)) = 
  compileBlock (BStmt line (Block line2 b))
compileStmt (Decl line t items) = do
  env <- compileDecl t items
  return (ReturnNothing, env)

-- todo compileStmt wszystkiego