module Frontend.Analyse where

import Latte.Abs
import Data.Map as Map 
import Frontend.Environment
import Frontend.Exception
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe

-- Memory Management -- 

alloc :: SAM Loc
alloc = do
  gets Map.size

insertValue :: Loc -> Val -> SAM ()
insertValue loc val = do
  store' <- gets (Map.insert loc val)
  put store'

declareVar :: Ident -> Val -> SAM ValEnv
declareVar id val = do
  loc <- alloc
  valEnv' <- asks (Map.insert id loc . fst)
  insertValue loc val
  return valEnv'

declareFunc :: Ident -> Func -> SAM FuncEnv
declareFunc id func = do
  asks (Map.insert id func . snd)

-- get values -- 

getFunc :: BNFC'Position -> Ident -> SAM Func
getFunc line id = do
  func <- asks (Map.lookup id . snd)
  case func of
    Just l -> return l
    Nothing -> throwError $ errMessage line (FunctionUndeclared id)

getIdentLoc :: BNFC'Position -> Ident -> SAM Loc
getIdentLoc line id = do
  loc <- asks (Map.lookup id . fst)
  case loc of
    Just l -> return l
    Nothing -> throwError $ errMessage line (VariableUndeclared id)

getLocVal :: BNFC'Position -> Loc -> SAM Val
getLocVal line loc = do
  val <- gets $ Map.lookup loc
  case val of
    Just v -> return v
    Nothing -> throwError "compiler internal error: getLocVal"

getIdentVal :: BNFC'Position -> Ident -> SAM Val
getIdentVal line id = do
  loc <- getIdentLoc line id
  getLocVal line loc

-- declare library functions --

libraryFunctions :: BNFC'Position -> [TopDef]
libraryFunctions l = 
  [ FnDef l (Void l) (Ident "printInt") [Arg l (Int l) (Ident "n")] (Block l []),
    FnDef l (Void l) (Ident "printString") [Arg l (Str l) (Ident "s")] (Block l []),
    FnDef l (Void l) (Ident "error") [] (Block l []),
    FnDef l (Int l) (Ident "readInt") [] (Block l []),
    FnDef l (Str l) (Ident "readString") [] (Block l []) ]

-- run analysis from main --

runMain :: Program -> SAM Val
runMain (Program line tds) = do
  env <- addTopDefs $ tds ++ libraryFunctions line
  local (const env) $ analyseExpr $ EApp line (Ident "main") []

-- TopDef --

addTopDef :: TopDef -> SAM Env
addTopDef (FnDef line t id args b) = do
  (valEnv, _) <- ask
  funcEnv <- declareFunc id (VFunc t id args b)
  return (valEnv, funcEnv)

addTopDefs :: [TopDef] -> SAM Env
addTopDefs [] = ask
addTopDefs (def:ds) = do
  newEnv <- addTopDef def
  local (const newEnv) $ addTopDefs ds

-- Expr Helpers --

declFunctionArgs :: BNFC'Position -> Ident -> [Expr] -> [Arg] -> SAM Env
declFunctionArgs _ _ [] [] = ask
declFunctionArgs line id [] (a:xa) = throwError $ errMessage line (FuncArgsNumberMismatch id)
declFunctionArgs line id (e:xe) [] = throwError $ errMessage line (FuncArgsNumberMismatch id)
declFunctionArgs line id (e:xe) ((Arg line2 t id2):xa) = do
  v <- analyseExpr e
  (_, funcEnv) <- ask
  valEnv' <- declareVar id2 v
  local (const (valEnv', funcEnv)) $ declFunctionArgs line id xe xa

analyseTwoIntExpr :: BNFC'Position -> Expr -> Expr -> SAM ()
analyseTwoIntExpr line expr1 expr2 = do
  e1 <- analyseExpr expr1
  e2 <- analyseExpr expr2
  case e1 of
    VInt -> do
      case e2 of
        VInt -> return ()
        _ -> throwError $ errMessage line ArithmOpTypeMismatch
    _ -> throwError $ errMessage line ArithmOpTypeMismatch

analyseTwoBoolExpr :: BNFC'Position -> Expr -> Expr -> SAM ()
analyseTwoBoolExpr line expr1 expr2 = do
  e1 <- analyseExpr expr1
  e2 <- analyseExpr expr2
  case e1 of
    VBool -> do
      case e2 of
        VBool -> return ()
        _ -> throwError $ errMessage line BoolOpTypeMismatch
    _ -> throwError $ errMessage line BoolOpTypeMismatch

analyseOneIntExpr :: BNFC'Position -> Expr -> SAM ()
analyseOneIntExpr line expr1 = do
  e1 <- analyseExpr expr1
  case e1 of
    VInt -> do
      return ()
    _ -> throwError $ errMessage line NonIntArgument

analyseOneBoolExpr :: BNFC'Position -> Expr -> SAM ()
analyseOneBoolExpr line expr1 = do
  e1 <- analyseExpr expr1
  case e1 of
    VBool -> do
      return ()
    _ -> throwError $ errMessage line NonBoolArgument

cmpTypeVal :: Type -> Val -> SAM Bool
cmpTypeVal t val = do
  case t of
    Int _ ->
      case val of
      VInt -> return True
      _ -> return False
    Bool _ ->
      case val of
      VBool -> return True
      _ -> return False
    Str _ ->
      case val of
      VString -> return True
      _ -> return False
    Void _ ->
      case val of
      VVoid -> return True
      _ -> return False
    _ -> throwError "compiler internal error: cmpTypeVal"

-- Analyse Expr --

analyseExpr :: Expr -> SAM Val
analyseExpr (EVar line id) = getIdentVal line id
analyseExpr (ELitInt line i) = return VInt
analyseExpr (ELitTrue line) = return VBool
analyseExpr (ELitFalse line) = return VBool

analyseExpr (EApp line id exprs) = do
  (VFunc t id args (Block line2 b)) <- getFunc line id
  env <- declFunctionArgs line id exprs args
  retVal <- local (const env) $ analyseBlock b
  case retVal of
    (Return val, _) -> do
      goodType <- cmpTypeVal t val
      if goodType then
        return val
      else
        throwError $ errMessage line (FuncWrongValueReturned id val)
    (ReturnNothing, _) -> do
      returnVoid <- cmpTypeVal t VVoid
      if returnVoid then 
        return VVoid
      else
        throwError $ errMessage line (FuncNoValueReturned id)

analyseExpr (EString line s) = return VString
analyseExpr (Neg line expr) = do
  analyseOneIntExpr line expr
  return VInt
analyseExpr (Not line expr) = do
  analyseOneBoolExpr line expr
  return VBool
analyseExpr (EMul line expr1 op expr2) = do
  analyseTwoIntExpr line expr1 expr2
  return VInt
analyseExpr (EAdd line expr1 op expr2) = do
  analyseTwoIntExpr line expr1 expr2
  return VInt
analyseExpr (EAnd line expr1 expr2) = do
  analyseTwoBoolExpr line expr1 expr2
  return VBool
analyseExpr (ERel line expr1 op expr2) = do
  analyseTwoIntExpr line expr1 expr2
  return VBool
analyseExpr (EOr line expr1 expr2) = do
  analyseTwoBoolExpr line expr1 expr2
  return VBool

-- Stmt --

analyseBlock :: [Stmt] -> SAM (RetInfo, Env)
analyseBlock [] = do
  env <- ask
  return (ReturnNothing, env)
analyseBlock (s:ss) = do
  ret <- analyseStmt s
  case ret of
    (Return val, env) -> return (Return val, env)
    (ReturnNothing, env) -> do
      local (const env) $ analyseBlock ss

declItem :: Type -> Item -> SAM Env
declItem t (NoInit line id) = do
  (_, funcEnv) <- ask
  n <- case t of
    Int line2 -> return VInt
    Bool line2 -> return VBool
    Str line2 -> return VString
  valEnv <- declareVar id n
  return (valEnv, funcEnv)
declItem t (Init line2 id e) = do
  (_, funcEnv) <- ask
  n <- analyseExpr e
  goodTypes <- cmpTypeVal t n
  if goodTypes then do
    valEnv <- declareVar id n
    return (valEnv, funcEnv)
  else
    throwError $ errMessage line2 (DeclTypeMismatch id)

analyseDecl :: Type -> [Item] -> SAM Env
analyseDecl t [] = ask
analyseDecl t (x:xs) = do
  env <- declItem t x
  local (const env) $ analyseDecl t xs

-- Execute Stmt -- 

analyseStmt :: Stmt -> SAM (RetInfo, Env)
analyseStmt (Empty line) = do
  env <- ask
  return (ReturnNothing, env)
analyseStmt (BStmt line (Block line2 b)) = analyseBlock b
analyseStmt (Decl line t items) = do
  env <- analyseDecl t items
  return (ReturnNothing, env)
analyseStmt (Ass line id expr) = do
  n <- analyseExpr expr
  l <- getIdentLoc line id
  env <- ask
  insertValue l n
  return (ReturnNothing, env)

analyseStmt (Incr line id) = do
  val <- getIdentVal line id
  case val of
    VInt -> do
      l <- getIdentLoc line id
      env <- ask
      insertValue l VInt
      return (ReturnNothing, env)
    _ -> throwError $ errMessage line NonIntArgument

analyseStmt (Decr line id) = do
  val <- getIdentVal line id
  case val of
    VInt -> do
      l <- getIdentLoc line id
      env <- ask
      insertValue l VInt
      return (ReturnNothing, env)
    _ -> throwError $ errMessage line NonIntArgument

analyseStmt (Ret line expr) = do
  e <- analyseExpr expr
  env <- ask
  return (Return e, env)
analyseStmt (VRet line) = do
  env <- ask
  return (Return VVoid, env)

analyseStmt (Cond line expr (BStmt _ (Block line2 block))) = do
  val <- analyseExpr expr 
  if val /= VBool then 
    throwError $ errMessage line $ ConditionNonBoolean val
  else do
    env <- ask
    case expr of 
      ELitTrue _ -> do
        (retVal, _) <- local (const env) $ analyseBlock block
        return (retVal, env)
      _ ->
        return (ReturnNothing, env) -- todo analizowac blok

analyseStmt (CondElse line expr (BStmt _ (Block line2 b1)) (BStmt _ (Block line3 b2))) = do
  val <- analyseExpr expr 
  if val /= VBool then 
    throwError $ errMessage line $ ConditionNonBoolean val
  else do
    env <- ask
    case expr of
      ELitTrue _ -> do 
        (retVal, _) <- local (const env) $ analyseBlock b1
        return (retVal, env)
      ELitFalse _ -> do
        (retVal, _) <- local (const env) $ analyseBlock b2
        return (retVal, env)
      _ -> 
        return (ReturnNothing, env) -- todo analizować bloki

analyseStmt (While line expr (BStmt _ (Block line2 block))) = do
  val <- analyseExpr expr 
  if val /= VBool then 
    throwError $ errMessage line $ ConditionNonBoolean val
  else do
    env <- ask
    case expr of 
      ELitTrue _ -> do
        (retVal, _) <- local (const env) $ analyseBlock block
        case retVal of
          Return val -> return (Return val, env)
          _ -> return (ReturnNothing, env)
      _ ->
        return (ReturnNothing, env) -- todo analizowac blok

analyseStmt (SExp line expr) = do
  env <- ask
  analyseExpr expr
  return (ReturnNothing, env)