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
  let strConstants = [] -- todo przeparsować values mapy
  let functions = sFunctions state
  return $ unlines $ printLLVMProgram strConstants functions

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

getIdentReg :: Ident -> CM Reg
getIdentReg id = do
  env <- ask
  let valEnv = eValEnv env
  let Just (reg, _) = Map.lookup id valEnv
  return reg

getFnType :: CM LLVM.Type
getFnType = do
  state <- get
  let f:functions = sFunctions state
  let t = fType f
  return t

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

-- emit load in order to read value from allocated register --
emitLoad :: LLVM.Type -> Reg -> CM Reg
emitLoad t reg = do
  reg2 <- newRegister
  emitStmt $ Load reg2 t (Ptr t) reg
  return reg2

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
  state <- get
  let functions = sFunctions state
  let function = Fn {
    fType = t,
    fName = name,
    fArgs = args,
    fBlocks = []
  }
  put $ state { sFunctions = function:functions}
  label <- newLabel
  emitNewBlock label
  setRegister $ Reg (toInteger $ length args + 1)
  emitArgsDecl (Reg 0) args

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
    -- todo moze sie zbuguje przy stringu
  newValEnv <- declareVarInEnv id reg
  return $ env { eValEnv = newValEnv }
emitDeclItem t (Init line id e) = do
  env <- ask
  llvmtype <- convTypeLLVMType t
  reg <- newRegister
  emitStmt $ Alloca reg llvmtype
  exprReg <- compileExpr e
  emitStmt $ Store llvmtype (VReg exprReg) (Ptr llvmtype) reg
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
  (retInfo, _) <- local (const env) $ compileBlock (BStmt line b)
  -- todo add ret void if no ret void 
  fnType <- getFnType
  when (fnType == Tvoid && retInfo == ReturnNothing) $ emitStmt RetVoid

-- compile exprs -- 
compileExpr :: Expr -> CM Reg
compileExpr (EVar _ id) = getIdentReg id
compileExpr (ELitInt _ i) = do
  reg <- newRegister
  emitStmt $ Alloca reg Ti32
  emitStmt $ Store Ti32 (VConst i) (Ptr Ti32) reg
  emitLoad Ti32 reg

-- todo reszta compileExpr

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
    (Return (t, reg), _) -> do
      case t of
        LLVM.Tvoid -> do
          emitStmt LLVM.RetVoid
        _ ->
          emitStmt $ LLVM.Ret t (VReg reg)
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

-- todo reszta compileStmt 

compileStmt (Latte.Ret _ expr) = do
  reg <- compileExpr expr
  env <- ask
  t <- getFnType
  return (Return (t, reg), env)
compileStmt (VRet _) = do -- todo nie wypisuje sie w void funkcjach bez returna
  env <- ask
  emitStmt RetVoid
  return (Return (Tvoid, Reg 0), env)

-- todo reszta compileStmt