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
    Nothing -> throwErrMessage line (FunctionUndeclared id)

getIdentLoc :: BNFC'Position -> Ident -> SAM Loc
getIdentLoc line id = do
  loc <- asks (Map.lookup id . fst)
  case loc of
    Just l -> return l
    Nothing -> throwErrMessage line (VariableUndeclared id)

getLocVal :: BNFC'Position -> Loc -> SAM Val
getLocVal line loc = do
  val <- gets $ Map.lookup loc
  case val of
    Just v -> return v
    Nothing -> Return (Val -1) -- todo czy na pewno tego potrzebuje

getIdentVal :: BNFC'Position -> Ident -> SAM Val
getIdentVal line id = do
  loc <- getIdentLoc line id
  getLocVal line loc

-- run analysis from main --

runMain :: Program -> SAM Val
runMain (Program line tds) = do
  env <- addTopDefs tds
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

negVInt :: Val -> Val
negVInt (VInt i) = VInt (-i)

notVBool :: Val -> Val
notVBool (VBool b) = VBool (not b)

relVInt :: RelOp -> Val -> Val -> Val
relVInt (LTH line) (VInt a) (VInt b) = VBool (a < b)
relVInt (LE line) (VInt a) (VInt b) = VBool (a <= b)
relVInt (GTH line) (VInt a) (VInt b) = VBool (a > b)
relVInt (GE line) (VInt a) (VInt b) = VBool (a >= b)
relVInt (EQU line) (VInt a) (VInt b) = VBool (a == b)
relVInt (NE line) (VInt a) (VInt b) = VBool (a /= b)

addVInt :: AddOp -> Val -> Val -> Val
addVInt (Plus line) (VInt a) (VInt b) = VInt (a + b)
addVInt (Minus line) (VInt a) (VInt b) = VInt (a - b)

mulVInt :: MulOp -> Val -> Val ->SAM Val
mulVInt (Times line) (VInt a) (VInt b) = return $ VInt (a * b)
mulVInt (Div line) (VInt a) (VInt b) =
  if b == 0 then
    throwError $ errMessage line "division by 0"
  else
    return $ VInt (a `div` b)
mulVInt (Mod line) (VInt a) (VInt b) =
  if b == 0 then
    throwError $ errMessage line "division by 0"
  else
    return $ VInt (a `mod` b)

andVBool :: Val -> Val -> Val
andVBool (VBool a) (VBool b) = VBool (a && b)

orVBool :: Val -> Val -> Val
orVBool (VBool a) (VBool b) = VBool (a || b)

declFunctionArgs :: BNFC'Position -> Ident -> [Expr] -> [Arg] -> SAM Env
declFunctionArgs _ _ [] [] = ask
declFunctionArgs line id [] (a:xa) = throwErrMessage line (FuncArgsNumberMismatch id)
declFunctionArgs line id (e:xe) [] = throwErrMessage line (FuncArgsNumberMismatch id)
-- todo ogarnac deklaracje argumentów w funkcji
-- declFunctionArgs _ id (e:xe) (a:xa) = do
--   v <- analyseExpr e
--   (_, funcEnv) <- ask
--   case a of
--     (ArgNoRef line t id) -> do
--       valEnv' <- declareVar id v
--       local (const (valEnv', funcEnv)) $ declFunctionArgs line xe xa
--     (ArgRef line t id) -> do
--       case e of
--         EVar line ident -> do
--           l <- getIdentLoc line ident
--           valEnv' <- asks (Map.insert id l . fst)
--           local (const (valEnv', funcEnv)) $ declFunctionArgs line xe xa
--         _ ->
--           throwError $ errMessage line "cannot pass given expression by reference"

analyseTwoIntExpr :: BNFC'Position -> Expr -> Expr -> SAM (Integer, Integer)
analyseTwoIntExpr line expr1 expr2 = do
  e1 <- analyseExpr expr1
  e2 <- analyseExpr expr2
  case e1 of
    (VInt i1) -> do
      case e2 of
        (VInt i2) -> return (i1, i2)
        _ -> throwError $ throwErrMessage ArithmOpTypeMismatch
    _ -> throwErrMessage ArithmOpTypeMismatch

analyseTwoBoolExpr :: BNFC'Position -> Expr -> Expr -> SAM (Bool, Bool)
analyseTwoBoolExpr line expr1 expr2 = do
  e1 <- analyseExpr expr1
  e2 <- analyseExpr expr2
  case e1 of
    (VBool b1) -> do
      case e2 of
        (VBool b2) -> return (b1, b2)
        _ -> throwErrMessage BoolOpTypeMismatch
    _ -> throw BoolOpTypeMismatch

analyseOneIntExpr :: BNFC'Position -> Expr -> SAM Integer
analyseOneIntExpr line expr1 = do
  e1 <- analyseExpr expr1
  case e1 of
    (VInt i1) -> do
      return i1
    _ -> throwErrMessage line NonIntArgument

analyseOneBoolExpr :: BNFC'Position -> Expr -> SAM Bool
analyseOneBoolExpr line expr1 = do
  e1 <- analyseExpr expr1
  case e1 of
    (VBool b1) -> do
      return b1
    _ -> throwErrMessage line NonBoolArgument

cmpTypeVal :: Type -> Val -> SAM Bool
cmpTypeVal t val = do
  case t of
    Int _ ->
      case val of
      (VInt _) -> return True
      _ -> return False
    Bool _ ->
      case val of
      (VBool _) -> return True
      _ -> return False
    Str _ ->
      case val of
      (VString _) -> return True
      _ -> return False
    Void _ ->
      case val of
      VVoid -> return True
      _ -> return False
    -- _ -> throwError $ errMessage Nothing "unknown type"

-- Analyse Expr --

analyseExpr :: Expr -> SAM Val
analyseExpr (EVar line id) = getIdentVal line id
analyseExpr (ELitInt line i) = return (VInt i)
analyseExpr (ELitTrue line) = return (VBool True)
analyseExpr (ELitFalse line) = return (VBool False)

analyseExpr (EApp line id exprs) = do
  (VFunc t id args (Block line2 b)) <- getFunc line id
  env <- declFunctionArgs line id exprs args
  retVal <- local (const env) $ execBlock b
  case retVal of
    (Return val, _) -> do
      goodType <- cmpTypeVal t val
      if goodType then
        return val
      else
        throwErrMessage line (FuncWrongValueReturned id val)
    _ -> throwErrMessage line (FuncNoValueReturned id)

analyseExpr (EString line s) = return (VString s)
analyseExpr (Neg line expr) = do
  i <- analyseOneIntExpr line expr
  return $ negVInt (VInt i)
analyseExpr (Not line expr) = do
  b <- analyseOneBoolExpr line expr
  return $ notVBool (VBool b)
analyseExpr (EMul line expr1 op expr2) = do
  (i1, i2) <- analyseTwoIntExpr line expr1 expr2
  mulVInt op (VInt i1) (VInt i2)
analyseExpr (EAdd line expr1 op expr2) = do
  (i1, i2) <- analyseTwoIntExpr line expr1 expr2
  return $ addVInt op (VInt i1) (VInt i2)
analyseExpr (EAnd line expr1 expr2) = do
  (b1, b2) <- analyseTwoBoolExpr line expr1 expr2
  return $ andVBool (VBool b1) (VBool b2)
analyseExpr (ERel line expr1 op expr2) = do
  (i1, i2) <- analyseTwoIntExpr line expr1 expr2
  return $ relVInt op (VInt i1) (VInt i2)
analyseExpr (EOr line expr1 expr2) = do
  (b1, b2) <- analyseTwoBoolExpr line expr1 expr2
  return $ orVBool (VBool b1) (VBool b2)

-- Stmt --

execBlock :: [Stmt] -> SAM (RetInfo, Env)
execBlock [] = do
  env <- ask
  return (ReturnNothing, env)
execBlock (s:ss) = do
  ret <- execStmt s
  case ret of
    (Return val, env) -> return (Return val, env)
    (ReturnNothing, env) -> do
      local (const env) $ execBlock ss
    (breakOrCont, env) -> return (breakOrCont, env)

declItem :: Type -> Item -> SAM Env
declItem t (NoInit line id) = do
  (_, funcEnv) <- ask
  n <- case t of
    Int line2 -> return (VInt 0)
    Bool line2 -> return (VBool False)
    Str line2 -> return (VString "")
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
    throwErrMessage line (DeclTypeMismatch id)

execDecl :: Type -> [Item] -> SAM Env
execDecl t [] = ask
execDecl t (x:xs) = do
  env <- declItem t x
  local (const env) $ execDecl t xs

-- Execute Stmt -- 

execStmt :: Stmt -> SAM (RetInfo, Env)
execStmt (Empty line) = do
  env <- ask
  return (ReturnNothing, env)
execStmt (BStmt line (Block line2 b)) = execBlock b
execStmt (Decl line t items) = do
  env <- execDecl t items
  return (ReturnNothing, env)
execStmt (Ass line id expr) = do
  n <- analyseExpr expr
  l <- getIdentLoc line id
  env <- ask
  insertValue l n
  return (ReturnNothing, env)

execStmt (Incr line id) = do
  val <- getIdentVal line id
  case val of
    (VInt v) -> do
      l <- getIdentLoc line id
      env <- ask
      insertValue l (VInt $ v + 1)
      return (ReturnNothing, env)
    _ -> throwErrMessage line NonIntArgument

execStmt (Decr line id) = do
  val <- getIdentVal line id
  case val of
    (VInt v) -> do
      l <- getIdentLoc line id
      env <- ask
      insertValue l (VInt $ v - 1)
      return (ReturnNothing, env)
    _ -> throwErrMessage line NonIntArgument

execStmt (Ret line expr) = do
  e <- analyseExpr expr
  env <- ask
  return (Return e, env)
execStmt (VRet line) = do
  env <- ask
  return (Return VVoid, env)
execStmt (Cond line expr (Block line2 block)) = do
  b <- analyseOneBoolExpr line expr
  env <- ask
  case b of 
    ELitTrue -> do
      (retVal, _) <- local (const env) $ execBlock block
      return (retVal, env)
    _ ->
      return (ReturnNothing, env)

execStmt (CondElse line expr (Block line2 b1) (Block line3 b2)) = do
  b <- analyseOneBoolExpr line expr
  env <- ask
  case b of
    ELitTrue -> do 
      (retVal, _) <- local (const env) $ execBlock b1
      return (retVal, env)
    ELitFalse -> do
      (retVal, _) <- local (const env) $ execBlock b2
      return (retVal, env)
    _ -> 
      return (ReturnNothing, env)

execStmt (While line expr (Block line2 block)) = do
  b <- analyseOneBoolExpr line expr
  env <- ask
  case b of 
    ELitTrue -> do
      (retVal, _) <- local (const env) $ execBlock block
      case retVal of
        Return val -> return (Return val, env)
        _ -> execStmt (While line expr (Block line2 block))
    _ ->
      return (ReturnNothing, env)

execStmt (SExp line expr) = do
  env <- ask
  analyseExpr expr
  return (ReturnNothing, env)