module Backend.CompileLLVM where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map              as Map

import           Backend.Environment
import           Backend.LLVM          as LLVM
import           Common.Runtime
import           Latte.Abs             as Latte
import           Optimizations.Mem2Reg

-- function for starting compilation and optimization --
runBackend :: Program -> CM LLVMProgram
runBackend (Program line tds) = do
  emitStrConst ""
  addFnTypesToState (tds ++ libraryFunctions line)
  env <- compileTopDefs tds
  state <- get
  return LLVMProgram {
    pStrConstants = sStrConstants state,
    pFunctions = sFunctions state,
    pCurrReg = sCurrReg state
  }


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
  let Just (t, reg, _, _) = Map.lookup id valEnv
  return (t, reg)

getIdentArrLength :: Ident -> CM ArrLength
getIdentArrLength id = do
  env <- ask
  let valEnv = eValEnv env
  let Just (_, _, _, l) = Map.lookup id valEnv
  return l

getLastBlock :: CM LLBlock
getLastBlock = do
  state <- get
  label <- getLabel
  let f:functions = sFunctions state
  let (Just b) = Map.lookup label (fBlocks f)
  return b

getCurrFnType :: CM LLVM.Type
getCurrFnType = do
  state <- get
  let f:functions = sFunctions state
  let t = fType f
  return t

setCurrFnMaxRegister :: Reg -> CM ()
setCurrFnMaxRegister reg = do
  state <- get
  let f:functions = sFunctions state
  let newF = f { fMaxRegister = reg }
  put $ state { sFunctions = newF:functions }

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
  let Just (_, _, scope, _) = Map.lookup id valEnv
  return scope

getLabel :: CM Label
getLabel = do
  gets sCurrLabel

putInBlockForBlock :: Label -> Label -> CM ()
putInBlockForBlock targetLabel label = do
  state <- get
  let f:functions = sFunctions state
  let (Just b) = Map.lookup targetLabel (fBlocks f)
  let inBlocks = bInBlocks b
  let block = b { bInBlocks = label:inBlocks}
  let function = f { fBlocks = Map.insert targetLabel block (fBlocks f) }
  put $ state { sFunctions = function:functions}
  return ()

putPhiForBlock :: Label -> Reg -> LLVM.Type -> (Val, Label) -> CM ()
putPhiForBlock label reg t phiVal = do
  state <- get
  let f:functions = sFunctions state
  let (Just b) = Map.lookup label (fBlocks f)
  let phis = case Map.lookup reg (bPhis b) of
        Nothing        -> []
        (Just (_, ps)) -> ps
  let block = b { bPhis = Map.insert reg (t, phiVal:phis) (bPhis b)}
  let function = f { fBlocks = Map.insert label block (fBlocks f) }
  put $ state { sFunctions = function:functions}
  return ()
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
  case t of
    _ -> do
      -- todo wyrzucic caly blok
      reg2 <- newRegister
      emitStmt $ Alloca reg2 t
      emitStmt $ Store t (VReg reg) (Ptr t) reg2
      newValEnv <- declareVarInEnv t (Ident strId) reg2 Nothing
      let (Reg r) = reg
      local (const (env {eValEnv = newValEnv})) $ emitArgsDecl (Reg (r + 1)) args
    _ -> do
      reg2 <- newRegister
      emitStmt $ Alloca reg2 t
      emitStmt $ Store t (VReg reg) (Ptr t) reg2
      newValEnv <- declareVarInEnv t (Ident strId) reg2 Nothing
      let (Reg r) = reg
      local (const (env {eValEnv = newValEnv})) $ emitArgsDecl (Reg (r + 1)) args

-- emit string constant --
emitStrConst :: String -> CM (Int, Int)
emitStrConst str = do
  state <- get
  let l = length str + 1
  let id = length (sStrConstants state)
  let strConst = StrConstant id l str
  put $ state { sStrConstants = strConst:sStrConstants state}
  return (id, l)

-- emit first label --
emitFirstLabel :: Label -> CM ()
emitFirstLabel label = do
  label2 <- newLabel
  emitNewBlock label2
  emitStmtForLabel (Br label2) (Label 0)
  putInBlockForBlock label2 (Label 0)

-- add function to compiler state --
emitFunction :: Label -> LLVM.Type -> String -> [(LLVM.Type, String)] -> CM Env
emitFunction label t name args = do
  state <- get
  let functions = sFunctions state
  let function = Fn {
    fType = t,
    fName = name,
    fArgs = args,
    fBlocks = Map.empty,
    fMaxRegister = Reg 0
  }
  put $ state {
    sFunctions = function:functions,
    sCurrReg = Reg 0
  }
  emitNewBlock label
  setRegister $ Reg (toInteger $ length args + 1)
  label2 <- getLabel
  emitFirstLabel label
  emitArgsDecl (Reg 0) args


-- add empty block to current function --
emitNewBlock :: Label -> CM ()
emitNewBlock label = do
  state <- get
  let f:functions = sFunctions state
  let blocks = fBlocks f
  let block = LLBlock {
    bLabel = label,
    bStmts = [],
    bPhis = Map.empty,
    bInBlocks = []
  }
  let function = f { fBlocks = Map.insert label block blocks }
  put $ state { sFunctions = function:functions }
  return ()

-- allocating and inserting registers to env and store without emmiting --
declareVarInEnv :: LLVM.Type -> Ident -> Reg -> Maybe ArrLength -> CM ValEnv
declareVarInEnv t id reg arrLength = do
  env <- ask
  let valEnv = eValEnv env
  let scope = eScope env
  let newValEnv = case arrLength of
          Nothing -> do
            Map.insert id (t, reg, scope, ArrLength (VConst 0)) valEnv
          (Just l) -> do
            Map.insert id (t, reg, scope, l) valEnv
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
    Str _ -> do
      emitStmt $ Store (Ptr Ti8) (VGetElementPtr 0 1 "") (Ptr (Ptr Ti8)) reg
    -- todo moze string?
  newValEnv <- declareVarInEnv llvmtype id reg Nothing
  return $ env { eValEnv = newValEnv }
emitDeclItem t (Init line id e) = do
  env <- ask
  llvmtype <- convTypeLLVMType t
  (_, exprVal) <- compileExpr e
  case t of
    (Array _ tArr) -> do
      let (VArr arrType arrSizeVal) = exprVal
      reg <- newRegister
      let sizeOf = case arrType of
            Ti32 -> 4
            Ti1 -> 1
            Ptr _ -> 8 -- todo sprawdzic sizeof pointera na stringi
      emitStmt $ Arithm reg Ti32 (VConst sizeOf) arrSizeVal Mul
      reg2 <- newRegister
      emitStmt $ Call reg2 (Ptr Ti8) "malloc" [(Ti32, VReg reg)]
      reg3 <- newRegister
      emitStmt $ Bitcast reg3 (Ptr Ti8) (VReg reg2) (Ptr arrType)
      newValEnv <- declareVarInEnv llvmtype id reg3 (Just (ArrLength arrSizeVal))
      return $ env { eValEnv = newValEnv}
    notArray -> do
      reg <- newRegister
      emitStmt $ Alloca reg llvmtype
      emitStmt $ Store llvmtype exprVal (Ptr llvmtype) reg
      newValEnv <- declareVarInEnv llvmtype id reg Nothing
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
    Array _ t -> do
      t' <- convTypeLLVMType t
      return $ Ptr t'

convArgTofArg :: Arg -> CM (LLVM.Type, String)
convArgTofArg (Arg _ t id) = do
  ident <- convIdentString id
  llvmtype <- convTypeLLVMType t
  return (llvmtype, ident)

convRelOpCond :: RelOp -> Cond
convRelOpCond relOp =
  case relOp of
    LTH _ -> RelSLT
    LE _  -> RelSLE
    GTH _ -> RelSGT
    GE _  -> RelSGE
    EQU _ -> RelEQ
    NE _  -> RelNE

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
  setRegister (Reg 0)
  label <- newLabel
  env <- emitFunction label llvmtype ident fArgs
  (retInfo, _) <- local (const env) $ compileBlock (BStmt line b)
  fnType <- getCurrFnType
  when (fnType == Tvoid && retInfo == ReturnNothing) $ do
    emitStmt RetVoid
  lastBlock <- getLastBlock
  when (Prelude.null (bStmts lastBlock)) $ do
    case llvmtype of
      Tvoid -> do
        emitStmt RetVoid
      Ti32 -> do
        emitStmt $ LLVM.Ret Ti32 (VConst 0)
      Ti1 -> do
        emitStmt $ LLVM.Ret Ti1 VFalse
      (Ptr Ti8) -> do
        emitStmt $ LLVM.Ret (Ptr Ti8) (VGetElementPtr 0 1 "")
      -- todo arrays
  reg <- getRegister
  --debugString $ show id
  --debugString $ show reg
  setCurrFnMaxRegister reg


-- compile list of expressions to val --
compileExprList :: [Expr] -> CM [Val]
compileExprList [] = return []
compileExprList (expr:exprs) = do
  (_, v) <- compileExpr expr
  valList <- compileExprList exprs
  return $ v : valList

-- get ident from array -- 
getIdentFromLIdx :: LValue -> CM Ident
getIdentFromLIdx (LIdx line expr _) = do
  case expr of
    (ELValue _ (LVar _ id)) ->
      return id

-- Analyse LValue --
getIdentLValue :: LValue -> CM Ident
getIdentLValue (LVar line id) = do
  return id
getIdentLValue (LIdx line expr _) = do
  case expr of
    (ELValue _ (LVar _ id)) ->
      return id

-- compile exprs --
-- always returns value or register which isn't a pointer --
compileExpr :: Expr -> CM (LLVM.Type, Val)
compileExpr (ELValue _ lvalue) = do
  id <- getIdentLValue lvalue
  (t, reg) <- getIdentTypeReg id
  reg2 <- emitLoad t reg
  -- todo sprawdzac czy tablice i emitoac loadArr
  return (t, VReg reg2)
compileExpr (ELitInt _ i) = do
  return (Ti32, VConst i)
compileExpr (ELitTrue _) = return (Ti1, VTrue)
compileExpr (ELitFalse _) = return (Ti1, VFalse)
compileExpr (EApp _ id exprs) = do
  let (Ident ident) = id
  t <- getFnType ident
  types <- getFnArgsTypes ident
  vals <- compileExprList exprs
  let llArgs = zip types vals
  case t of
    Tvoid -> do
      emitStmt $ CallVoid t ident llArgs
      return (Ti32, VConst 0)
    _ -> do
      reg <- newRegister
      emitStmt $ Call reg t ident llArgs
      return (t, VReg reg)
compileExpr (EString _ s) = do
  (id, l) <- emitStrConst s
  return (Ptr Ti8, VGetElementPtr id l s)
compileExpr (Neg _ expr) = do
  (_, e) <- compileExpr expr
  reg <- newRegister
  emitStmt $ Arithm reg Ti32 (VConst 0) e Sub
  return (Ti1, VReg reg)
compileExpr (Not _ expr) = do
  (_, e) <- compileExpr expr
  reg <- newRegister
  emitStmt $ Xor reg Ti1 e VTrue
  return (Ti1, VReg reg)
compileExpr (EMul _ expr1 op expr2) = do
  (_, e1) <- compileExpr expr1
  (_, e2) <- compileExpr expr2
  reg <- newRegister
  case op of
    (Times _)     -> emitStmt $ Arithm reg Ti32 e1 e2 Mul
    (Latte.Div _) -> emitStmt $ Arithm reg Ti32 e1 e2 LLVM.Div
    (Mod _)       -> emitStmt $ Arithm reg Ti32 e1 e2 Rem
  return (Ti32, VReg reg)
compileExpr (EAdd _ expr1 op expr2) = do
  (t1, e1) <- compileExpr expr1
  (t2, e2) <- compileExpr expr2
  case t1 of
    Ti32 -> do
      reg <- newRegister
      case op of
        (Plus _)  -> emitStmt $ Arithm reg Ti32 e1 e2 Add
        (Minus _) -> emitStmt $ Arithm reg Ti32 e1 e2 Sub
      return (Ti32, VReg reg)
    _ -> do
      reg <- newRegister
      emitStmt $ Call reg (Ptr Ti8) "__concatStrings__" [(t1, e1), (t2, e2)]
      return (Ptr Ti8, VReg reg)
compileExpr (EAnd l expr1 expr2) = do
  compileExpr $ Not l (EOr l (Not l expr1) (Not l expr2))
compileExpr (EOr _ expr1 expr2) = do
  (_, e1) <- compileExpr expr1
  lStart <- getLabel
  lFalse <- newLabel
  emitNewBlock lFalse
  (_, e2) <- compileExpr expr2
  lFalse2 <- getLabel
  lTrue <- newLabel
  emitStmtForLabel (Br lTrue) lFalse2
  putInBlockForBlock lTrue lFalse2
  emitNewBlock lTrue
  emitStmtForLabel (BrCond Ti1 e1 lTrue lFalse) lStart
  putInBlockForBlock lTrue lStart
  putInBlockForBlock lFalse lStart
  reg <- newRegister
  --emitStmt $ Phi reg Ti1 [(VTrue, lStart), (e2, lFalse2)]
  lCurr <- getLabel
  putPhiForBlock lCurr reg Ti1 (VTrue, lStart)
  putPhiForBlock lCurr reg Ti1 (e2, lFalse2)
  return (Ti1, VReg reg)
compileExpr (ERel _ expr1 op expr2) = do
  let cond = convRelOpCond op
  (t1, e1) <- compileExpr expr1
  (t2, e2) <- compileExpr expr2
  reg <- newRegister
  case t1 of
    Ti1 -> do
      emitStmt $ Cmp reg cond Ti1 e1 e2
      return (Ti1, VReg reg)
    Ti32 -> do
      emitStmt $ Cmp reg cond Ti32 e1 e2
      return (Ti1, VReg reg)
    (Ptr Ti8) -> do
      emitStmt $ Call reg Ti32 "__equStrings__" [(Ptr Ti8, e1), (Ptr Ti8, e2)]
      reg2 <- newRegister
      emitStmt $ Cmp reg2 cond Ti32 (VConst 1) (VReg reg)
      return (Ptr Ti8, VReg reg2)
-- arr
compileExpr (ENewArr _ t expr) = do
  t' <- convTypeLLVMType t
  (_, e) <- compileExpr expr
  return (t', VArr t' e)
compileExpr (ELength line lvalue) = do
  (t, e) <- compileExpr lvalue
  case e of
    (VArr t len) -> return (Ti32, len)
    reg -> do
      case lvalue of 
        ELValue l lval -> do
          id <- getIdentLValue lval
          (ArrLength len) <- getIdentArrLength id
          return (Ti32, len)

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
          newRegister
          emitStmt LLVM.RetVoid
        _ -> do
          newRegister
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
compileStmt (BStmt line (Block line2 b)) = do
  env <- ask
  (retInfo, _) <- compileBlock (BStmt line (Block line2 b))
  return (retInfo, env)
compileStmt (Decl line t items) = do
  env <- compileDecl t items
  return (ReturnNothing, env)
compileStmt (Ass _ lvalue expr) = do
  env <- ask
  id <- getIdentLValue lvalue
  (_, e) <- compileExpr expr
  (t, r) <- getIdentTypeReg id
  emitStmt $ Store t e (Ptr t) r
  return (ReturnNothing, env)
compileStmt (Incr l lvalue) = do
  env <- ask
  id <- getIdentLValue lvalue
  (_, r) <- getIdentTypeReg id
  (_, reg) <- compileExpr (EAdd l (ELValue l (LVar l id)) (Plus l) (ELitInt l 1))
  emitStmt $ Store Ti32 reg (Ptr Ti32) r
  return (ReturnNothing, env)
compileStmt (Decr l lvalue) = do
  env <- ask
  id <- getIdentLValue lvalue
  (_, r) <- getIdentTypeReg id
  (_, reg) <- compileExpr (EAdd l (ELValue l (LVar l id)) (Minus l) (ELitInt l 1))
  emitStmt $ Store Ti32 reg (Ptr Ti32) r
  return (ReturnNothing, env)
compileStmt (Latte.Ret _ expr) = do
  (_, val) <- compileExpr expr
  env <- ask
  t <- getCurrFnType
  return (Return (t, val), env)
compileStmt (VRet _) = do
  env <- ask
  return (Return (Tvoid, VConst 0), env)
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
      (_, e) <- compileExpr expr
      lStart <- getLabel
      lTrue <- newLabel
      emitNewBlock lTrue
      local (const env) $ compileBlock block
      lTrue2 <- getLabel
      lFalse <- newLabel
      emitNewBlock lFalse
      emitStmtForLabel (BrCond Ti1 e lTrue lFalse) lStart
      putInBlockForBlock lTrue lStart
      putInBlockForBlock lFalse lStart
      emitStmtForLabel (Br lFalse) lTrue2
      putInBlockForBlock lFalse lTrue2
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
      (_, e) <- compileExpr expr
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
      putInBlockForBlock lTrue lStart
      putInBlockForBlock lFalse lStart
      emitStmtForLabel (Br lEnd) lTrue2
      putInBlockForBlock lEnd lTrue2
      emitStmtForLabel (Br lEnd) lFalse2
      putInBlockForBlock lEnd lFalse2
      return (ReturnNothing, env)
  return (ReturnNothing, env)
compileStmt (While _ expr block) = do
  env <- ask
  lStart0 <- getLabel
  lStart <- newLabel
  emitNewBlock lStart
  (_, e) <- compileExpr expr
  lStart2 <- getLabel
  lBlock <- newLabel
  emitNewBlock lBlock
  local (const env) $ compileBlock block
  emitStmt $ Br lStart
  lCurr <- getLabel
  putInBlockForBlock lStart lCurr
  lEnd <- newLabel
  emitNewBlock lEnd
  emitStmtForLabel (Br lStart) lStart0
  putInBlockForBlock lStart lStart0
  emitStmtForLabel (BrCond Ti1 e lBlock lEnd) lStart2
  putInBlockForBlock lBlock lStart2
  putInBlockForBlock lEnd lStart2
  return (ReturnNothing, env)
-- todo dla jednego statementa
compileStmt (ForEach l t id expr (BStmt _ (Block _ blockStmts))) = do
  (Reg r) <- getRegister
  let i = Ident $ "while" ++ show r
  env <- ask
  let firstBlockStmt =
          Decl l
            (Int l) [
              Init l id
              (ELValue l (LIdx l expr (ELValue l (LVar l i))))
            ]
  let lastBlockStmt = Incr l (LVar l i)
  let loopBlockStmts =
        [firstBlockStmt] ++ blockStmts ++ [lastBlockStmt]
  let forEachStmt =
        BStmt l
        (Block l [
          Decl l (Int l) [Init l i (ELitInt l 0)],
          While l (ERel l (ELValue l (LVar l i)) (LTH l) (ELength l expr))
            (BStmt l (Block l loopBlockStmts))
        ])
  compileStmt forEachStmt
