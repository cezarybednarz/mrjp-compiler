module Frontend.Analyse where

import           Common.Runtime
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map             as Map
import           Data.Maybe
import           Frontend.Environment
import           Frontend.Exception
import           Latte.Abs

-- run analysis from main --

runMain :: Program -> SAM ()
runMain (Program line tds) = do
  env <- addTopDefs $ tds ++ libraryFunctions line
  local (const env) $ analyseMain line
  local (const env) $ analyseTopDefs tds
  return ()

-- analyse main --

analyseMain :: BNFC'Position -> SAM ()
analyseMain line = do
  (VFunc t _ args _) <- getFunc line (Ident "main")
  tv <- convTypeVal t
  when (tv /= VInt) $ throwError $ errMessage line MainWrongType
  when (length args /= 0) $ throwError $ errMessage line MainWrongNumberOfArgs

-- memory management --

alloc :: SAM Loc
alloc = do
  gets Map.size

insertValue :: Loc -> Val -> SAM ()
insertValue loc val = do
  store' <- gets (Map.insert loc val)
  put store'

declareVar :: BNFC'Position -> Ident -> Val -> SAM ValEnv
declareVar line id val = do
  (valEnv, _, _, scope1) <- ask
  if notMember id valEnv then do
    loc <- alloc
    valEnv' <- asks (Map.insert id (loc, scope1) . fst4)
    insertValue loc val
    return valEnv'
  else do
    (Just (loc, scope2)) <- asks (Map.lookup id . fst4)
    if scope1 == scope2 then do
      throwError $ errMessage line (VariableRedeclared id)
    else do
      loc <- alloc
      valEnv' <- asks (Map.insert id (loc, scope1) . fst4)
      insertValue loc val
      return valEnv'


declareFunc :: BNFC'Position -> Ident -> Func -> SAM FuncEnv
declareFunc line id func = do
  (_, funcEnv, _, _) <- ask
  if member id funcEnv then
    throwError $ errMessage line (FunctionRedeclared id)
  else
    asks (Map.insert id func . snd4)

analyseReassignment :: BNFC'Position -> Ident -> Loc -> Val -> SAM ()
analyseReassignment line id loc val = do
  storeVal <- getIdentVal line id
  when (val /= storeVal) $ throwError $ errMessage line (AssTypeMismatch id)

-- get values --

getFunc :: BNFC'Position -> Ident -> SAM Func
getFunc line id = do
  func <- asks (Map.lookup id . snd4)
  case func of
    Just l  -> return l
    Nothing -> throwError $ errMessage line (FunctionUndeclared id)

getIdentLoc :: BNFC'Position -> Ident -> SAM Loc
getIdentLoc line id = do
  loc <- asks (Map.lookup id . fst4)
  case loc of
    Just (l, _) -> return l
    Nothing     -> throwError $ errMessage line (VariableUndeclared id)

getLocVal :: BNFC'Position -> Loc -> SAM Val
getLocVal line loc = do
  val <- gets $ Map.lookup loc
  case val of
    Just v  -> return v
    Nothing -> throwError "compiler internal error: getLocVal"

getIdentVal :: BNFC'Position -> Ident -> SAM Val
getIdentVal line id = do
  loc <- getIdentLoc line id
  getLocVal line loc

-- TopDef --

addTopDef :: TopDef -> SAM Env
addTopDef (FnDef line t id args b) = do
  (valEnv, _, fnRetVal, scope) <- ask
  funcEnv <- declareFunc line id (VFunc t id args b)
  return (valEnv, funcEnv, fnRetVal, scope)

addTopDefs :: [TopDef] -> SAM Env
addTopDefs [] = ask
addTopDefs (def:ds) = do
  newEnv <- addTopDef def
  local (const newEnv) $ addTopDefs ds

convArgExpr :: Arg -> SAM Expr
convArgExpr (Arg _ t _) =
  case t of
    (Bool _) -> return $ ELitTrue Nothing
    (Int _)  -> return $ ELitInt Nothing 0
    (Str _)  -> return $ EString Nothing ""
    _        -> throwError "compiler internal error: convArgExpr"

analyseTopDef :: TopDef -> SAM ()
analyseTopDef (FnDef line t id args b) = do
  argExprs <- mapM convArgExpr args
  fnRetVal <- convTypeVal t
  (_, funcEnv, _, scope) <- ask
  (valEnv, _, _, _) <- declFunctionArgs line id argExprs args
  (blockRetVal, _) <- local (const (valEnv, funcEnv, fnRetVal, scope)) $ analyseBlock (BStmt line b)
  case fnRetVal of
    VVoid -> return ()          -- ok, void
    _ ->
      case blockRetVal of
        ReturnNothing -> do     -- error, expecting to return some value
          throwError $ errMessage line (FuncNoValueReturned id)
        _ ->
          return ()             -- ok, good value returned (value checked before in analyseBlock)

analyseTopDefs :: [TopDef] -> SAM [()]
analyseTopDefs = mapM analyseTopDef

-- Expr Helpers --

declFunctionArgs :: BNFC'Position -> Ident -> [Expr] -> [Arg] -> SAM Env
declFunctionArgs _ _ [] [] = ask
declFunctionArgs line id [] (a:xa) = throwError $ errMessage line (FuncArgsNumberMismatch id)
declFunctionArgs line id (e:xe) [] = throwError $ errMessage line (FuncArgsNumberMismatch id)
declFunctionArgs line id (e:xe) ((Arg line2 t id2):xa) = do
  val <- analyseExpr e
  argVal <- convTypeVal t
  if val /= argVal then
    throwError $ errMessage line (ArgTypeMismatch id id2)
  else do
    (valEnv, funcEnv, fnRetVal, scope) <- ask
    valEnv' <- local (const (valEnv, funcEnv, fnRetVal, scope+1)) $ declareVar line2 id2 val
    local (const (valEnv', funcEnv, fnRetVal, scope)) $ declFunctionArgs line id xe xa

analyseValInTwoExpr :: BNFC'Position -> Val -> Expr -> Expr -> SAM ()
analyseValInTwoExpr line val expr1 expr2 = do
  e1 <- analyseExpr expr1
  e2 <- analyseExpr expr2
  when ((e1 /= val) || (e2 /= val)) $ throwError $ errMessage line (OpTypeMismatch val)

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

convTypeVal :: Type -> SAM Val
convTypeVal t = do
  case t of
    Int _ ->
      return VInt
    Bool _ ->
      return VBool
    Str _ ->
      return VString
    Void _ ->
      return VVoid
    _ -> throwError "compiler internal error: convTypeVal"

cmpTypeVal :: Type -> Val -> SAM Bool
cmpTypeVal t val = do
  tVal <- convTypeVal t
  return $ tVal == val

-- Analyse LValue --

-- analyseLValue :: LValue -> SAM Val
-- analyseLValue (LVar line id) = getIdentVal line

-- Analyse Expr --

analyseExpr :: Expr -> SAM Val
analyseExpr (L line id) = getIdentVal line id
analyseExpr (ELitInt line i) = return VInt
analyseExpr (ELitTrue line) = return VBool
analyseExpr (ELitFalse line) = return VBool

analyseExpr (EApp line id exprs) = do
  (VFunc t id args block) <- getFunc line id
  env <- declFunctionArgs line id exprs args
  convTypeVal t

analyseExpr (EString line s) = return VString
analyseExpr (Neg line expr) = do
  analyseOneIntExpr line expr
  return VInt
analyseExpr (Not line expr) = do
  analyseOneBoolExpr line expr
  return VBool
analyseExpr (EMul line expr1 op expr2) = do
  analyseValInTwoExpr line VInt expr1 expr2
  return VInt
analyseExpr (EAdd line expr1 op expr2) = do
  e <- analyseExpr expr1
  case e of
    VInt    -> analyseValInTwoExpr line VInt expr1 expr2
    VString -> do
      case op of
        Plus _ -> analyseValInTwoExpr line VString expr1 expr2
        _      -> throwError $ errMessage line InvalidStringOperator
    _       -> analyseValInTwoExpr line VInt expr1 expr2
  return e
analyseExpr (EAnd line expr1 expr2) = do
  analyseValInTwoExpr line VBool expr1 expr2
  return VBool
analyseExpr (ERel line expr1 op expr2) = do
  e <- analyseExpr expr1
  case e of
    VInt  -> analyseValInTwoExpr line VInt expr1 expr2
    VBool -> analyseValInTwoExpr line VBool expr1 expr2
    VString -> case op of
                 Latte.Abs.EQU _ -> analyseValInTwoExpr line VString expr1 expr2
                 Latte.Abs.NE _ -> analyseValInTwoExpr line VString expr1 expr2
                 _ -> throwError $ errMessage line StringInvalidRel
    _     -> analyseValInTwoExpr line VInt expr1 expr2
  return VBool
analyseExpr (EOr line expr1 expr2) = do
  analyseValInTwoExpr line VBool expr1 expr2
  return VBool

-- Stmt --

analyseBlock :: Stmt -> SAM (RetInfo, Env)
analyseBlock block = do
  (valEnv, funcEnv, fnRetVal, scope) <- ask
  case block of
    BStmt line (Block line2 b) -> do
      local (const (valEnv, funcEnv, fnRetVal, scope+1)) $ analyseBlockStmts b
    stmt ->
      local (const (valEnv, funcEnv, fnRetVal, scope+1)) $ analyseBlockStmts [stmt]

analyseBlockStmts :: [Stmt] -> SAM (RetInfo, Env)
analyseBlockStmts [] = do
  env <- ask
  return (ReturnNothing, env)
analyseBlockStmts (s:ss) = do
  (_, _, fnRetVal, _) <- ask
  ret <- analyseStmt s
  case ret of
    (Return val, env) -> do
      if val /= fnRetVal then do
        throwError $ errMessage (hasPosition s) (FuncWrongValueReturned fnRetVal)
      else
        return (Return val, env)
    (ReturnNothing, env) -> do
      local (const env) $ analyseBlockStmts ss

declItem :: Type -> Item -> SAM Env
declItem t (NoInit line id) = do
  (_, funcEnv, fnRetVal, scope) <- ask
  n <- case t of
    Int line2  -> return VInt
    Bool line2 -> return VBool
    Str line2  -> return VString
    Void line2 -> throwError $ errMessage line2 VoidVaribaleDeclaration
  valEnv <- declareVar line id n
  return (valEnv, funcEnv, fnRetVal, scope)
declItem t (Init line id e) = do
  (_, funcEnv, fnRetVal, scope) <- ask
  n <- analyseExpr e
  goodTypes <- cmpTypeVal t n
  if goodTypes then do
    valEnv <- declareVar line id n
    return (valEnv, funcEnv, fnRetVal, scope)
  else
    throwError $ errMessage line (DeclTypeMismatch id)

analyseDecl :: Type -> [Item] -> SAM Env
analyseDecl t [] = ask
analyseDecl t (x:xs) = do
  env <- declItem t x
  local (const env) $ analyseDecl t xs

analyseBoolCondition :: BNFC'Position -> Expr -> SAM ()
analyseBoolCondition line expr = do
  val <- analyseExpr expr
  when (val /= VBool) $ throwError $ errMessage line ConditionNonBoolean

-- Execute Stmt --

analyseStmt :: Stmt -> SAM (RetInfo, Env)
analyseStmt (Empty line) = do
  env <- ask
  return (ReturnNothing, env)
analyseStmt (BStmt line (Block line2 b)) = analyseBlock (BStmt line (Block line2 b))
analyseStmt (Decl line t items) = do
  env <- analyseDecl t items
  return (ReturnNothing, env)
analyseStmt (Ass line id expr) = do
  n <- analyseExpr expr
  l <- getIdentLoc line id
  env <- ask
  analyseReassignment line id l n
  insertValue l n
  return (ReturnNothing, env)

analyseStmt (Incr line id) = do
  val <- getIdentVal line id
  case val of
    VInt -> do
      l <- getIdentLoc line id
      env <- ask
      analyseReassignment line id l VInt
      insertValue l VInt
      return (ReturnNothing, env)
    _ -> throwError $ errMessage line NonIntArgument

analyseStmt (Decr line id) =
  analyseStmt (Incr line id)

analyseStmt (Ret line expr) = do
  e <- analyseExpr expr
  env <- ask
  return (Return e, env)
analyseStmt (VRet line) = do
  env <- ask
  return (Return VVoid, env)

analyseStmt (Cond line expr block) = do
  analyseBoolCondition line expr
  env <- ask
  case expr of
    ELitTrue _ -> do
      (retVal, _) <- local (const env) $ analyseBlock block
      return (retVal, env)
    ELitFalse _ ->
      return (ReturnNothing, env)
    _ -> do
      local (const env) $ analyseBlock block
      return (ReturnNothing, env)

analyseStmt (CondElse line expr block1 block2) = do
  analyseBoolCondition line expr
  env <- ask
  case expr of
    ELitTrue _ -> do
      (retVal, _) <- local (const env) $ analyseBlock block1
      return (retVal, env)
    ELitFalse _ -> do
      (retVal, _) <- local (const env) $ analyseBlock block2
      return (retVal, env)
    _ -> do
      (retVal1, _) <- local (const env) $ analyseBlock block1
      (retVal2, _) <- local (const env) $ analyseBlock block2
      case retVal1 of
        ReturnNothing ->
          return (ReturnNothing, env)
        _ ->
          case retVal2 of
            ReturnNothing ->
              return (ReturnNothing, env)
            _ -> return (retVal1, env)

analyseStmt (While line expr block) =
  analyseStmt (Cond line expr block)

analyseStmt (SExp line expr) = do
  env <- ask
  analyseExpr expr
  return (ReturnNothing, env)
