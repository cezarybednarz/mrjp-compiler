{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Backend.CompileLLVM where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import           Data.Map             as Map

import Backend.Environment
import Backend.LLVM as LLVM
import Latte.Abs as Latte


-- function for starting compilation --
runMain :: Program -> CM String
runMain (Program line tds) = do
  env <- compileTopDefs tds
  state <- get
  let functions = sFunctions state
  return $ show functions -- todo przeparsowac llvm

-- compiler state modifiers --

-- register and label setters -- 
newRegister :: CM Reg
newRegister = do
  state <- get
  let (Reg reg) = sCurrReg state
  put $ state { sCurrReg = Reg (reg + 1)}
  return $ Reg reg

newLabel :: CM Label
newLabel = do
  state <- get
  let (Label label) = sCurrLabel state
  put $ state { sCurrLabel = Label (label + 1)}
  return $ Label label

setRegister :: Reg -> CM ()
setRegister reg = do
  state <- get
  put $ state { sCurrReg = reg }

setLabel :: Label -> CM ()
setLabel label = do
  state <- get
  put $ state { sCurrLabel = label }

getRegister :: CM Reg
getRegister = do
  gets sCurrReg

-- emit instruction to current block in current function -- 
emitStmt :: LLVMStmt -> CM ()
emitStmt llvmstmt = do
  state <- get
  let f:functions = sFunctions state
  let b:blocks = fBlocks f
  let llvmstmts = bStmts b
  let block = b { bStmts = llvmstmt:llvmstmts }
  let function = f { fBlocks = block:blocks }
  put $ state { sFunctions = function:functions }
  return ()

-- emit arguments declaration -- 
emitArgsDecl :: Reg -> [(LLVM.Type, String)] -> CM Env
emitArgsDecl reg [] = ask
emitArgsDecl reg ((t, strId):args) = do
  env <- ask 
  reg2 <- newRegister
  emitStmt $ Alloca reg2 t
  emitStmt $ Store t (VReg reg) (Ptr t) reg2
  newValEnv <- declareVarInEnv (Ident strId) reg
  let (Reg r) = reg
  local (const (env {eValEnv = newValEnv})) $ emitArgsDecl (Reg (r + 1)) args

-- add function to compiler state --
emitFunction :: LLVM.Type -> String -> [(LLVM.Type, String)] -> CM Env
emitFunction t name args = do
  setRegister $ Reg (toInteger $ length args + 1)
  env <- emitArgsDecl (Reg 0) args
  state <- get
  let functions = sFunctions state
  let function = Fn {
    fType = t,
    fName = name,
    fArgs = args,
    fBlocks = []
  }
  put $ state { sFunctions = function:functions}
  return env

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

-- allocating and inserting registers to env and store without emmiting --

declareVarInEnv :: Ident -> Reg -> CM ValEnv
declareVarInEnv id reg = do
  env <- ask
  let valEnv = eValEnv env
  let scope = eScope env
  let newValEnv = Map.insert id (reg, scope) valEnv
  return newValEnv


-- emit declaration of item --
emitDeclItem :: Latte.Type -> Item -> CM Env
emitDeclItem t (NoInit line id) = do
  env <- ask
  llvmtype <- convTypeLLVMType t
  reg <- newRegister
  emitStmt $ Alloca reg llvmtype
  case t of
    Int _ -> do
      emitStmt $ Store Ti32 (VConst 0) (Ptr Ti32) reg
    Bool _ -> do
      emitStmt $ Store Ti1 (VConst 0) (Ptr Ti1) reg
  newValEnv <- declareVarInEnv id reg
  return $ env { eValEnv = newValEnv }
emitDeclItem t (Init line id e) = do
  env <- ask
  llvmtype <- convTypeLLVMType t
  reg <- newRegister
  emitStmt $ Alloca reg llvmtype
  -- todo dopisac ewaluacje expression
  return env

-- convert between latte and llvm types --
convIdentString :: Ident -> CM String
convIdentString (Ident id) = return id

convTypeLLVMType :: Latte.Type -> CM LLVM.Type
convTypeLLVMType t = do
  case t of
    Int _ ->
      return Ti32
    Bool _ ->
      return Ti1
    Str _ ->
      return $ Ptr Ti8
    Void _ ->
      return Tvoid

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

-- compile stmts helpers --
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
  env <- emitDeclItem t x
  local (const env) $ compileDecl t xs

-- compile stmts-- 
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