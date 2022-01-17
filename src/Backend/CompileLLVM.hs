module Backend.CompileLLVM where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import           Data.Map             as Map

import Backend.Environment
import Backend.LLVM as LLVM
import Latte.Abs as Latte
import Common.Runtime

-- function for starting compilation --
runMain :: Program -> CM String
runMain (Program line tds) = do
  addFnTypesToState (tds ++ libraryFunctions line)
  state <- get
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
  (Reg reg) <- newRegister
  setLabel (Label reg)
  return $ Label reg

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

getIdentTypeReg :: Ident -> CM (LLVM.Type, Reg)
getIdentTypeReg id = do
  env <- ask
  let valEnv = eValEnv env
  let Just (t, reg, _) = Map.lookup id valEnv
  return (t, reg)

getCurrFnType :: CM LLVM.Type
getCurrFnType = do
  state <- get
  let f:functions = sFunctions state
  let t = fType f
  return t

getFnType :: String -> CM LLVM.Type
getFnType name = do
  state <- get
  let functionTypes = sFunctionTypes state
  let Just (t, _) = Map.lookup name functionTypes
  return t

getFnArgsTypes :: String -> CM [LLVM.Type]
getFnArgsTypes name = do
  state <- get
  let functionTypes = sFunctionTypes state
  let Just (_, args) = Map.lookup name functionTypes
  return args

getScope :: Ident -> CM Scope
getScope id = do
  env <- ask
  let valEnv = eValEnv env
  let Just (_, _, scope) = Map.lookup id valEnv
  return scope

getLabel :: CM Label
getLabel = do
  gets sCurrLabel

-- emit instruction to current block in current function -- 
emitStmt :: LLVMStmt -> CM ()
emitStmt llvmstmt = do
  label <- getLabel
  emitStmtForLabel llvmstmt label
  return ()

emitStmtForLabel :: LLVMStmt -> Label -> CM ()
emitStmtForLabel llvmstmt label = do
  state <- get
  let f:functions = sFunctions state
  let (Just b) = Map.lookup label (fBlocks f)
  let llvmstmts = bStmts b
  let block = b { bStmts = llvmstmt:llvmstmts }
  let function = f { fBlocks = Map.insert label block (fBlocks f)  }
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
  newValEnv <- declareVarInEnv t (Ident strId) reg2
  let (Reg r) = reg
  local (const (env {eValEnv = newValEnv})) $ emitArgsDecl (Reg (r + 1)) args

-- add function to compiler state --
emitFunction :: Label -> LLVM.Type -> String -> [(LLVM.Type, String)] -> CM Env
emitFunction label t name args = do
  state <- get
  let functions = sFunctions state
  let function = Fn {
    fType = t,
    fName = name,
    fArgs = args,
    fBlocks = Map.empty
  }
  put $ state {
    sFunctions = function:functions
  }
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
  let function = f { fBlocks = Map.insert label block blocks }
  put $ state { sFunctions = function:functions }
  return ()

-- allocating and inserting registers to env and store without emmiting --
declareVarInEnv :: LLVM.Type -> Ident -> Reg -> CM ValEnv
declareVarInEnv t id reg = do
  env <- ask
  let valEnv = eValEnv env
  let scope = eScope env
  let newValEnv = Map.insert id (t, reg, scope) valEnv
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
  newValEnv <- declareVarInEnv llvmtype id reg
  return $ env { eValEnv = newValEnv }
emitDeclItem t (Init line id e) = do
  env <- ask
  llvmtype <- convTypeLLVMType t
  reg <- newRegister
  emitStmt $ Alloca reg llvmtype
  exprVal <- compileExpr e
  emitStmt $ Store llvmtype exprVal (Ptr llvmtype) reg
  newValEnv <- declareVarInEnv llvmtype id reg
  return $ env { eValEnv = newValEnv }

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

-- add function types to compiler state --
addFnTypesToState :: [TopDef] -> CM ()
addFnTypesToState [] = return ()
addFnTypesToState ((FnDef line t id args b):fns) = do
  fArgs <- mapM convArgTofArg args
  llvmtype <- convTypeLLVMType t
  let (Ident name) = id
  state <- get
  let functionTypes = sFunctionTypes state
  put $ state {
    sFunctionTypes = Map.insert name (llvmtype, Prelude.map fst fArgs) functionTypes
  }
  addFnTypesToState fns

-- compile topdefs --
compileTopDefs :: [TopDef] -> CM [()]
compileTopDefs = mapM compileTopDef

compileTopDef :: TopDef -> CM ()
compileTopDef (FnDef line t id args b) = do
  fArgs <- mapM convArgTofArg args
  ident <- convIdentString id
  llvmtype <- convTypeLLVMType t
  label <- newLabel
  env <- emitFunction label llvmtype ident fArgs
  (retInfo, _) <- local (const env) $ compileBlock (BStmt line b)
  fnType <- getCurrFnType
  when (fnType == Tvoid && retInfo == ReturnNothing) $ emitStmt RetVoid

-- compile list of expressions to val --
compileExprList :: [Expr] -> CM [Val]
compileExprList [] = return []
compileExprList (expr:exprs) = do
  v <- compileExpr expr
  valList <- compileExprList exprs
  return $ v : valList

-- compile exprs -- 
-- always returns value or register which isn't a pointer --
compileExpr :: Expr -> CM Val
compileExpr (EVar _ id) = do
  (t, reg) <- getIdentTypeReg id
  reg2 <- emitLoad t reg
  return $ VReg reg2
compileExpr (ELitInt _ i) = do
  return $ VConst i
compileExpr (ELitTrue _) = return VTrue
compileExpr (ELitFalse _) = return VFalse
compileExpr (EApp _ id exprs) = do
  let (Ident ident) = id
  t <- getFnType ident
  types <- getFnArgsTypes ident
  vals <- compileExprList exprs
  let llArgs = zip types vals
  case t of
    Tvoid -> do
      emitStmt $ CallVoid t ident llArgs
      return VNull
    _ -> do
      reg <- newRegister
      emitStmt $ Call reg t ident llArgs
      return $ VReg reg
compileExpr (EString _ s) = return VNull -- todo
compileExpr (Neg _ expr) = do
  e <- compileExpr expr
  reg <- newRegister
  emitStmt $ Arithm reg Ti32 (VConst 0) e Sub
  return (VReg reg)
compileExpr (Not _ expr) = do
  e <- compileExpr expr
  reg <- newRegister
  emitStmt $ Xor reg Ti1 e VTrue
  return (VReg reg)
compileExpr (EMul _ expr1 op expr2) = do
  e1 <- compileExpr expr1
  e2 <- compileExpr expr2
  reg <- newRegister
  case op of
    (Times _) -> emitStmt $ Arithm reg Ti32 e1 e2 Mul
    (Latte.Div _) -> emitStmt $ Arithm reg Ti32 e1 e2 LLVM.Div
    (Mod _) -> emitStmt $ Arithm reg Ti32 e1 e2 Rem
  return (VReg reg)
compileExpr (EAdd _ expr1 op expr2) = do
  e1 <- compileExpr expr1
  e2 <- compileExpr expr2
  -- todo dodawanie stringow (case na typ Val e1, czy ptr czy nie)
  reg <- newRegister
  case op of
    (Plus _) -> emitStmt $ Arithm reg Ti32 e1 e2 Add
    (Minus _) -> emitStmt $ Arithm reg Ti32 e1 e2 Sub
  return (VReg reg)
compileExpr (EAnd l expr1 expr2) = do
  compileExpr $ Not l (EOr l (Not l expr1) (Not l expr2))
compileExpr (EOr _ expr1 expr2) = do
  e1 <- compileExpr expr1
  lStart <- getLabel
  lFalse <- newLabel
  emitNewBlock lFalse
  e2 <- compileExpr expr2
  lFalse2 <- getLabel
  lTrue <- newLabel
  emitStmtForLabel (Br lTrue) lFalse2
  emitNewBlock lTrue
  emitStmtForLabel (BrCond Ti1 e1 lTrue lFalse) lStart
  reg <- newRegister
  emitStmt $ Phi reg Ti1 [(VTrue, lStart), (e2, lFalse2)]
  return (VReg reg)
compileExpr (ERel _ expr1 op expr2) = return VNull -- todo


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
    (Return (t, val), _) -> do
      case t of
        LLVM.Tvoid -> do
          emitStmt LLVM.RetVoid
        _ ->
          emitStmt $ LLVM.Ret t val
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
compileStmt (Ass _ id expr) = do
  env <- ask
  e <- compileExpr expr
  (t, r) <- getIdentTypeReg id
  emitStmt $ Store t e (Ptr t) r
  return (ReturnNothing, env)
compileStmt (Incr l id) = do
  env <- ask
  (_, r) <- getIdentTypeReg id
  reg <- compileExpr (EAdd l (EVar l id) (Plus l) (ELitInt l 1))
  emitStmt $ Store Ti32 reg (Ptr Ti32) r
  return (ReturnNothing, env)
compileStmt (Decr l id) = do
  env <- ask
  (_, r) <- getIdentTypeReg id
  reg <- compileExpr (EAdd l (EVar l id) (Minus l) (ELitInt l 1))
  emitStmt $ Store Ti32 reg (Ptr Ti32) r
  return (ReturnNothing, env)
compileStmt (Latte.Ret _ expr) = do
  val <- compileExpr expr
  env <- ask
  t <- getCurrFnType
  return (Return (t, val), env)
compileStmt (VRet _) = do
  env <- ask
  emitStmt RetVoid
  return (Return (Tvoid, VNull), env)
compileStmt (SExp _ expr) = do
  env <- ask
  compileExpr expr
  return (ReturnNothing, env)
compileStmt (Cond _ expr block) = do
  env <- ask
  case expr of
    ELitTrue _ -> do
      (retVal, _) <- local (const env) $ compileBlock block
      return (retVal, env)
    ELitFalse _ -> do
      return (ReturnNothing, env)
    _ -> do
      e <- compileExpr expr
      lStart <- getLabel
      lTrue <- newLabel
      emitNewBlock lTrue
      local (const env) $ compileBlock block
      lTrue2 <- getLabel
      lFalse <- newLabel
      emitNewBlock lFalse
      emitStmtForLabel (BrCond Ti1 e lTrue lFalse) lStart
      emitStmtForLabel (Br lFalse) lTrue2
      return (ReturnNothing, env)
compileStmt (CondElse _ expr block1 block2) = do
  env <- ask
  case expr of 
    ELitTrue _ -> do 
      (retVal, _) <- local (const env) $ compileBlock block1
      return (retVal, env)
    ELitFalse _ -> do
      (retVal, _) <- local (const env) $ compileBlock block2
      return (retVal, env)
    _ -> do
      e <- compileExpr expr
      lStart <- getLabel
      lTrue <- newLabel
      emitNewBlock lTrue
      local (const env) $ compileBlock block1
      lTrue2 <- getLabel
      lFalse <- newLabel
      emitNewBlock lFalse
      local (const env) $ compileBlock block2
      lFalse2 <- getLabel
      lEnd <- newLabel
      emitNewBlock lEnd
      emitStmtForLabel (BrCond Ti1 e lTrue lFalse) lStart
      emitStmtForLabel (Br lEnd) lTrue2
      emitStmtForLabel (Br lEnd) lFalse2
      return (ReturnNothing, env)
  return (ReturnNothing, env)
compileStmt (While _ expr block) = do
  env <- ask
  return (ReturnNothing, env) -- todo