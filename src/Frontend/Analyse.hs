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
    Nothing -> throwErrMessage line (Undefined id)

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
  local (const env) $ evalExpr $ EApp line (Ident "main") []

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
--   v <- evalExpr e
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

evalTwoIntExpr :: BNFC'Position -> Expr -> Expr -> SAM (Integer, Integer)
evalTwoIntExpr line expr1 expr2 = do
  e1 <- evalExpr expr1
  e2 <- evalExpr expr2
  case e1 of
    (VInt i1) -> do
      case e2 of
        (VInt i2) -> return (i1, i2)
        _ -> throwError $ throwErrMessage ArithmOpTypeMismatch
    _ -> throwErrMessage ArithmOpTypeMismatch

evalTwoBoolExpr :: BNFC'Position -> Expr -> Expr -> SAM (Bool, Bool)
evalTwoBoolExpr line expr1 expr2 = do
  e1 <- evalExpr expr1
  e2 <- evalExpr expr2
  case e1 of
    (VBool b1) -> do
      case e2 of
        (VBool b2) -> return (b1, b2)
        _ -> throwErrMessage BoolOpTypeMismatch
    _ -> throw BoolOpTypeMismatch

evalOneIntExpr :: BNFC'Position -> Expr -> SAM Integer
evalOneIntExpr line expr1 = do
  e1 <- evalExpr expr1
  case e1 of
    (VInt i1) -> do
      return i1
    _ -> throwError $ errMessage line "argument should be integer" -- todo tutaj skonczylem

evalOneBoolExpr :: BNFC'Position -> Expr -> SAM Bool
evalOneBoolExpr line expr1 = do
  e1 <- evalExpr expr1
  case e1 of
    (VBool b1) -> do
      return b1
    _ -> throwError $ errMessage line "argument should be boolean"

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
    _ -> throwError $ errMessage Nothing "unknown type"

-- Evaluate Expr --

evalExpr :: Expr -> SAM Val
evalExpr (EVar line id) = getIdentVal line id
evalExpr (ELitInt line i) = return (VInt i)
evalExpr (ELitTrue line) = return (VBool True)
evalExpr (ELitFalse line) = return (VBool False)

evalExpr (EApp line id exprs) = do
  (VFunc t id args (Block line2 b)) <- getFunc line id
  env <- declFunctionArgs line id exprs args
  retVal <- local (const env) $ execBlock b
  case retVal of
    (Return val, _) -> do
      goodType <- cmpTypeVal t val
      if goodType then
        return val
      else
        throwError $ errMessage line $ "function " ++ show id ++ " returns wrong value type"
    _ -> throwError $ errMessage line $ "function " ++ show id ++ "didn't return anything"

evalExpr (EString line s) = return (VString s)
evalExpr (Neg line expr) = do
  i <- evalOneIntExpr line expr
  return $ negVInt (VInt i)
evalExpr (Not line expr) = do
  b <- evalOneBoolExpr line expr
  return $ notVBool (VBool b)
evalExpr (EMul line expr1 op expr2) = do
  (i1, i2) <- evalTwoIntExpr line expr1 expr2
  mulVInt op (VInt i1) (VInt i2)
evalExpr (EAdd line expr1 op expr2) = do
  (i1, i2) <- evalTwoIntExpr line expr1 expr2
  return $ addVInt op (VInt i1) (VInt i2)
evalExpr (EAnd line expr1 expr2) = do
  (b1, b2) <- evalTwoBoolExpr line expr1 expr2
  return $ andVBool (VBool b1) (VBool b2)
evalExpr (ERel line expr1 op expr2) = do
  (i1, i2) <- evalTwoIntExpr line expr1 expr2
  return $ relVInt op (VInt i1) (VInt i2)
evalExpr (EOr line expr1 expr2) = do
  (b1, b2) <- evalTwoBoolExpr line expr1 expr2
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
  n <- evalExpr e
  goodTypes <- cmpTypeVal t n
  if goodTypes then do
    valEnv <- declareVar id n
    return (valEnv, funcEnv)
  else
    throwError $ errMessage line2 "declaration types don't match"

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
  n <- evalExpr expr
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
    _ -> throwError $ errMessage line "argument should be integer"

execStmt (Decr line id) = do
  val <- getIdentVal line id
  case val of
    (VInt v) -> do
      l <- getIdentLoc line id
      env <- ask
      insertValue l (VInt $ v - 1)
      return (ReturnNothing, env)
    _ -> throwError $ errMessage line "argument should be integer"

execStmt (Ret line expr) = do
  e <- evalExpr expr
  env <- ask
  return (Return e, env)
execStmt (VRet line) = do
  env <- ask
  return (Return VVoid, env)
execStmt (Cond line expr (Block line2 block)) = do
  b <- evalOneBoolExpr line expr
  env <- ask
  if b then do
    (retVal, _) <- local (const env) $ execBlock block
    return (retVal, env)
  else
    return (ReturnNothing, env)

execStmt (CondElse line expr (Block line2 b1) (Block line3 b2)) = do
  b <- evalOneBoolExpr line expr
  env <- ask
  if b then do
    (retVal, _) <- local (const env) $ execBlock b1
    return (retVal, env)
  else do
    (retVal, _) <- local (const env) $ execBlock b2
    return (retVal, env)


execStmt (While line expr (Block line2 block)) = do
  b <- evalOneBoolExpr line expr
  env <- ask
  if b then do
    (retVal, _) <- local (const env) $ execBlock block
    case retVal of
      Return val -> return (Return val, env)
      _ -> execStmt (While line expr (Block line2 block))
  else
    return (ReturnNothing, env)

execStmt (SExp line expr) = do
  env <- ask
  evalExpr expr
  return (ReturnNothing, env)