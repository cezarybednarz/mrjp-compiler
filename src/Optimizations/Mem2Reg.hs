module Optimizations.Mem2Reg (runMem2Reg) where

import Latte.Abs as Latte
import Backend.LLVM as LLVM
import Control.Monad.State
import Data.Map as Map

-- Modern SSA Algorithm
-- https://pp.info.uni-karlsruhe.de/uploads/publikationen/braun13cc.pdf

-- type reg --
data TypeReg = TypeReg LLVM.Type Reg
  deriving (Show, Ord, Eq)

-- mem2reg state monad --
data OptimizerState = OptimizerState {
  sProgram :: LLVMProgram,
  sNewProgram :: LLVMProgram,
  sCurrentFn :: Maybe Fn,
  sCurrentDef :: Map.Map Reg (Map.Map Label TypeReg)
}

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
  gets sProgram

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

putEmptyPhiForBlock :: Label -> TypeReg -> OM ()
putEmptyPhiForBlock label (TypeReg t reg) = do
  b <- getBlock label
  f <- getCurrentFn
  state <- get
  let block = b { bPhis = Map.insert reg (t, []) (bPhis b) }
  let newFunction = f { fBlocks = Map.insert label block (fBlocks f) }
  setCurrentFn newFunction
  return ()

putPhiForBlock :: Label -> TypeReg -> (Val, Label) -> OM ()
putPhiForBlock label (TypeReg t reg) phiVal = do
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
  sProgram = llvmProgram,
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
  state <- get
  newProgram <- getNewProgram
  optimizedFunctions <- optimizeFns (pFunctions newProgram)
  setNewProgram (newProgram { pFunctions = optimizedFunctions })
  return $ sNewProgram state

-- functions for optimizing structures in LLVM --
optimizeStmt :: Label -> LLVMStmt -> OM (Maybe LLVMStmt)
optimizeStmt label llvmstmt = do
  case llvmstmt of
    (Call r1 t1 s1 ts) -> return $ Just (Call r1 t1 s1 ts)
    (CallVoid t1 s1 ts) -> return $ Just (CallVoid t1 s1 ts)
    (LLVM.Ret t1 v1) -> return $ Just (LLVM.Ret t1 v1)
    RetVoid -> return $ Just RetVoid
    (Arithm r1 t1 v1 v2 op) -> do
      v1' <- readVal (t1, v1) label
      v2' <- readVal (t1, v2) label
      return $ Just (Arithm r1 t1 v1' v2' op)
    (Br l1) -> return $ Just (Br l1)
    (BrCond t1 v1 l1 l2) -> do
      v1' <- readVal (t1, v1) label
      return $ Just (BrCond t1 v1' l1 l2)
    (Load {}) -> return Nothing -- todo moze write variable
    (Store {}) -> return Nothing -- todo moze write variable
    (Alloca {}) -> return Nothing -- todo moze write vairable
    (Cmp r1 c1 t1 v1 v2) -> do
      v1' <- readVal (t1, v1) label
      v2' <- readVal (t1, v2) label
      return $ Just (Cmp r1 c1 t1 v1' v2')
    (Xor r1 t1 v1 v2) -> do
      v1' <- readVal (t1, v1) label
      v2' <- readVal (t1, v2) label
      return $ Just (Xor r1 t1 v1' v2')
    Phi {} -> return Nothing -- phis are stored in map inside block

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

optimizeBlock :: LLBlock -> OM LLBlock
optimizeBlock block = do
  let stmts = reverse $ bStmts block
  let label = bLabel block
  newStmts <- optimizeStmtsList label stmts
  return $ block { bStmts = newStmts }

optimizeCurrFn :: OM ()
optimizeCurrFn = do
  clearCurrentDef
  fn <- getCurrentFn
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
writeVariable :: TypeReg -> Label -> TypeReg -> OM ()
writeVariable (TypeReg t targetReg) label val = do
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
readVal :: (LLVM.Type, Val) -> Label -> OM Val
readVal (t, val) label = do
  case val of
    (VReg reg) -> do
      (TypeReg _ optimizedReg) <- readVariable (TypeReg t reg) label
      return $ VReg optimizedReg
    val' -> return val'

-- read register from block (from modern SSA algorithm) --
readVariable :: TypeReg -> Label -> OM TypeReg
readVariable (TypeReg t reg) label = do
  state <- get
  let currentDef = sCurrentDef state
  case Map.lookup reg currentDef of
    (Just innerMap) -> do
      case Map.lookup label innerMap of
        (Just val) ->
          return val
        Nothing ->
          readVariableRecursive (TypeReg t reg) label
    Nothing -> do
      readVariableRecursive (TypeReg t reg) label

-- read register recursively through predecessor blocks (from modern SSA algorithm) --
readVariableRecursive :: TypeReg -> Label -> OM TypeReg
readVariableRecursive (TypeReg t reg) label = do
  block <- getBlock label
  val <- case bInBlocks block of
    [inBlockLabel] -> do -- single predecessor, no phi
      readVariable (TypeReg t reg) label
    inBlockLabels -> do
      regPhi <- newRegister
      putEmptyPhiForBlock label (TypeReg t regPhi)
      writeVariable (TypeReg t reg) label (TypeReg t regPhi)
      addPhiOperands (TypeReg t reg) label (TypeReg t regPhi)
  writeVariable (TypeReg t reg) label val
  return val

-- add phi operand (from modern SSA algorithm)--
addPhiOperands :: TypeReg -> Label -> TypeReg -> OM TypeReg
addPhiOperands variable label (TypeReg _ regPhi) = do
  block <- getBlock label
  let preds = bInBlocks block
  mapM_ (appendOperands label variable regPhi) preds
  return variable

-- helper function for addPhiOperands --
appendOperands :: Label -> TypeReg -> Reg -> Label -> OM ()
appendOperands label typeReg val label2 = do
  putPhiForBlock label typeReg (VReg val, label2)