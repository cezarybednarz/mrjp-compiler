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

clearBlock :: LLBlock -> OM LLBlock
clearBlock block = do
  return $ block { bStmts = [] }

clearFn :: Fn -> OM Fn
clearFn fn = do
  let (labels, blocks) = Prelude.unzip $ Map.toList (fBlocks fn)
  clearBlocks <- mapM clearBlock blocks
  let newFBlocks = Map.fromList $ Prelude.zip labels clearBlocks
  return $ fn { fBlocks = newFBlocks }

clearNewProgram :: OM ()
clearNewProgram = do
  program <- getNewProgram
  let functions = pFunctions program
  newFunctions <- mapM clearFn functions  
  let newProgram = program { pFunctions = newFunctions }
  state <- get
  put $ state { sNewProgram = newProgram}
  return ()

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
  clearNewProgram
  -- todo odpalić przetwarzanie funkcji
  return $ sNewProgram state

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

-- read register from block --
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

-- read register recursively through predecessor blocks --
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

-- add phi operand --
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