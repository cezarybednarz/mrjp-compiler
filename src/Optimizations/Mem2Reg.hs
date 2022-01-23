module Optimizations.Mem2Reg (runMem2Reg) where

import Latte.Abs as Latte
import Backend.LLVM as LLVM
import Control.Monad.State
import Data.Map as Map

-- Modern SSA Algorithm
-- https://pp.info.uni-karlsruhe.de/uploads/publikationen/braun13cc.pdf

-- type reg --
data TypeVal = TypeVal LLVM.Type Val
  deriving (Show, Ord, Eq)

-- mem2reg state monad --
data OptimizerState = OptimizerState {
  sNewProgram :: LLVMProgram,
  sCurrentFn :: Maybe Fn,
  sCurrentDef :: Map.Map Reg (Map.Map Label TypeVal)
}
  deriving (Show, Eq, Ord)

-- acessors to state -- 
newRegister :: OM Reg
newRegister = do
  program <- getNewProgram
  state <- get
  let (Reg reg) = pCurrReg program
  put $ state { sNewProgram = (program { pCurrReg = Reg (reg + 1)}) }
  return $ Reg reg

getNewProgram :: OM LLVMProgram
getNewProgram =
  gets sNewProgram

setNewProgram :: LLVMProgram -> OM ()
setNewProgram program = do
  state <- get
  put $ state { sNewProgram = program }

getCurrentFn :: OM Fn
getCurrentFn = do
  state <- get
  let (Just fn) = sCurrentFn state
  return fn

setCurrentFn :: Fn -> OM ()
setCurrentFn f = do
  state <- get
  put $ state { sCurrentFn = Just f }
  return ()

getBlock :: Label -> OM LLBlock
getBlock label = do
  function <- getCurrentFn
  let (Just block) = Map.lookup label (fBlocks function)
  return block

putEmptyPhiForBlock :: Label -> LLVM.Type -> Reg -> OM ()
putEmptyPhiForBlock label t reg = do
  b <- getBlock label
  f <- getCurrentFn
  state <- get
  let block = b { bPhis = Map.insert reg (t, []) (bPhis b) }
  let newFunction = f { fBlocks = Map.insert label block (fBlocks f) }
  setCurrentFn newFunction
  return ()

putPhiForBlock :: Label -> LLVM.Type -> Reg -> (Val, Label) -> OM ()
putPhiForBlock label t reg phiVal = do
  b <- getBlock label
  f <- getCurrentFn
  state <- get
  let (Just (_, phis)) = Map.lookup reg (bPhis b)
  let block = b { bPhis = Map.insert reg (t, phiVal:phis) (bPhis b)}
  let newFunction = f { fBlocks = Map.insert label block (fBlocks f) }
  setCurrentFn newFunction
  return ()

clearCurrentDef :: OM ()
clearCurrentDef = do
  state <- get
  put $ state { sCurrentDef = Map.empty }

-- optimizer monad -- 
type OM a = (StateT OptimizerState IO) a

-- initial state 
initOptimizerState :: LLVMProgram -> OptimizerState
initOptimizerState llvmProgram = OptimizerState {
  sNewProgram = llvmProgram,
  sCurrentFn = Nothing,
  sCurrentDef = Map.empty
}

-- run optimizer monad -- 
runOM :: OM a -> OptimizerState -> IO (a, OptimizerState)
runOM = runStateT

-- start optimizing program (entrypoint of )--
runMem2Reg :: LLVMProgram -> IO (LLVMProgram, OptimizerState)
runMem2Reg llvmProgram =
  runOM runOptimization (initOptimizerState llvmProgram)

-- run optimization -- 
runOptimization :: OM LLVMProgram
runOptimization = do
  newProgram <- getNewProgram
  optimizedFunctions <- optimizeFns (pFunctions newProgram)
  setNewProgram (newProgram { pFunctions = optimizedFunctions })
  gets sNewProgram

-- functions for optimizing structures in LLVM --
optimizeStmt :: Label -> LLVMStmt -> OM (Maybe LLVMStmt)
optimizeStmt label llvmstmt = do
  --debugString $ "  " ++ show llvmstmt
  case llvmstmt of
    (Call r1 t1 s1 ts) -> do
      ts' <- optimizeArgsList label ts
      writeVariable r1 label (TypeVal t1 (VReg r1))
      return $ Just (Call r1 t1 s1 ts)
    (CallVoid t1 s1 ts) -> do
      ts' <- optimizeArgsList label ts
      return $ Just (CallVoid t1 s1 ts')
    (LLVM.Ret t1 v1) -> do
      v1' <- readVal (TypeVal t1 v1) label
      return $ Just (LLVM.Ret t1 v1')
    RetVoid -> return $ Just RetVoid
    (Arithm r1 t1 v1 v2 op) -> do
      v1' <- readVal (TypeVal t1 v1) label
      v2' <- readVal (TypeVal t1 v2) label
      writeVariable r1 label (TypeVal t1 (VReg r1))
      return $ Just (Arithm r1 t1 v1' v2' op)
    (Br l1) -> return $ Just (Br l1)
    (BrCond t1 v1 l1 l2) -> do
      v1' <- readVal (TypeVal t1 v1) label
      return $ Just (BrCond t1 v1' l1 l2)
    (Load r1 t1 t2 r2) -> do
      r2Val' <- readVal (TypeVal t1 (VReg r2)) label
      writeVariable r1 label (TypeVal t1 r2Val')
      return Nothing
    (Store t1 v1 t2 r) -> do
      writeVariable r label (TypeVal t1 v1)
      return Nothing
    Alloca {} -> return Nothing
    (Cmp r1 c1 t1 v1 v2) -> do
      v1' <- readVal (TypeVal t1 v1) label
      v2' <- readVal (TypeVal t1 v2) label
      writeVariable r1 label (TypeVal t1 (VReg r1))
      return $ Just (Cmp r1 c1 t1 v1' v2')
    (Xor r1 t1 v1 v2) -> do
      v1' <- readVal (TypeVal t1 v1) label
      v2' <- readVal (TypeVal t1 v2) label
      writeVariable r1 label (TypeVal t1 (VReg r1))
      return $ Just (Xor r1 t1 v1' v2')
    Phi {} -> return Nothing -- phis are stored in map inside block

optimizeArgsList :: Label -> [(LLVM.Type, Val)] -> OM [(LLVM.Type, Val)]
optimizeArgsList label [] = return []
optimizeArgsList label ((t, val):args) = do
  val' <- readVal (TypeVal t val) label
  optimizedArgsList <- optimizeArgsList label args
  return $ (t, val) : optimizedArgsList

optimizeStmtsList :: Label -> [LLVMStmt] -> OM [LLVMStmt]
optimizeStmtsList _ [] = return []
optimizeStmtsList label (stmt:stmts) = do
  finalStmtsList <- optimizeStmtsList label stmts
  optimizedStmt <- optimizeStmt label stmt
  case optimizedStmt of
    (Just llvmstmt) -> do
      return $ llvmstmt : finalStmtsList
    Nothing -> do
      return finalStmtsList

-- insert to state phis generated by backend --
optimizeBackendPhis :: Label -> LLBlock -> OM ()
optimizeBackendPhis label block = do
  let (phiRegs, phiVals) = Prelude.unzip (Map.toList (bPhis block))
  let (phiTypes, _) = Prelude.unzip phiVals
  let phis = Prelude.zip phiTypes phiRegs
  mapM_ (\(t, r) -> writeVariable r label (TypeVal t (VReg r))) phis

optimizeBlock :: LLBlock -> OM LLBlock
optimizeBlock block = do
  --debugString "-----"
  let stmts = bStmts block
  let label = bLabel block
  optimizeBackendPhis label block
  newStmts <- optimizeStmtsList label stmts
  return $ block { bStmts = newStmts }

optimizeCurrFn :: OM ()
optimizeCurrFn = do
  clearCurrentDef
  fn <- getCurrentFn
  --debugString $ "function " ++ fName fn ++ "()"
  let (labels, blocks) = Prelude.unzip $ Map.toList $ fBlocks fn
  newBlocks <- mapM optimizeBlock blocks
  setCurrentFn $ fn { fBlocks = Map.fromList (Prelude.zip labels newBlocks)}
  return ()

optimizeFns :: [Fn] -> OM [Fn]
optimizeFns [] = return []
optimizeFns (fn:fns) = do
  setCurrentFn fn
  optimizeCurrFn
  newFn <- getCurrentFn
  finalOptimizedFns <- optimizeFns fns
  return $ newFn : finalOptimizedFns

-- assign register to block -- 
writeVariable :: Reg -> Label -> TypeVal -> OM ()
writeVariable targetReg label val = do
  state <- get
  let currentDef = sCurrentDef state
  let newCurrentDef = case Map.lookup targetReg currentDef of
        Nothing -> do
          Map.insert targetReg (Map.fromList [(label, val)]) currentDef
        (Just innerMap) -> do
          Map.insert targetReg (Map.insert label val innerMap) currentDef
  put $ state { sCurrentDef = newCurrentDef }
  return ()

-- read value from block -- 
readVal :: TypeVal -> Label -> OM Val
readVal (TypeVal t val) label = do
  case val of
    (VReg reg) -> do
      (TypeVal _ optimizedVal) <- readVariable t reg label
      return optimizedVal
    val' -> return val'

-- read register from block (from modern SSA algorithm) --
readVariable :: LLVM.Type -> Reg -> Label -> OM TypeVal
readVariable t reg label = do
  --debugState
  state <- get
  let currentDef = sCurrentDef state
  case Map.lookup reg currentDef of
    (Just innerMap) -> do
      case Map.lookup label innerMap of
        (Just val) ->
          return val
        Nothing ->
          readVariableRecursive t reg label
    Nothing -> do
      readVariableRecursive t reg label

-- read register recursively through predecessor blocks (from modern SSA algorithm) --
readVariableRecursive :: LLVM.Type -> Reg -> Label -> OM TypeVal
readVariableRecursive t reg label = do
  block <- getBlock label
  val <- case bInBlocks block of
    [inBlockLabel] -> do -- single predecessor, no phi
      readVariable t reg label
    inBlockLabels -> do
      regPhi <- newRegister
      putEmptyPhiForBlock label t regPhi
      writeVariable reg label (TypeVal t (VReg regPhi))
      addPhiOperands reg label (TypeVal t (VReg regPhi))
  writeVariable reg label val
  return val

-- add phi operand (from modern SSA algorithm)--
addPhiOperands :: Reg -> Label -> TypeVal -> OM TypeVal
addPhiOperands variable label (TypeVal t valPhi) = do
  block <- getBlock label
  let preds = bInBlocks block
  mapM_ (appendOperands label variable (TypeVal t valPhi)) preds
  return (TypeVal t (VReg variable))

-- helper function for addPhiOperands --
appendOperands :: Label -> Reg -> TypeVal -> Label -> OM ()
appendOperands label typeReg (TypeVal t val) label2 = do
  putPhiForBlock label t typeReg (val, label2)

-- ! debug functions 

debugState :: OM ()
debugState = do
  state <- get
  liftIO $ print state

debugString :: String -> OM ()
debugString str = do
  liftIO $ print str