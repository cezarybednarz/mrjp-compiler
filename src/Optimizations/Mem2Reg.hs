module Optimizations.Mem2Reg (runMem2Reg) where

import Latte.Abs as Latte
import Backend.LLVM as LLVM
import Control.Monad.State
import Data.Map as Map

-- Modern SSA Algorithm
-- https://pp.info.uni-karlsruhe.de/uploads/publikationen/braun13cc.pdf

-- mem2reg state monad --
data OptimizerState = OptimizerState {
  sProgram :: LLVMProgram,
  sNewProgram :: LLVMProgram,
  sCurrentFn :: Maybe Fn,
  sCurrentDef :: Map.Map Reg (Map.Map Label Reg) -- todo add type
}

-- acessors to state -- 
getProgram :: OM LLVMProgram
getProgram =
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

putPhiForBlock :: Label -> Reg -> LLVM.Type -> (Val, Label) -> OM ()
putPhiForBlock label reg t phiVal = do
  b <- getBlock label
  f <- getCurrentFn
  state <- get 
  let (Just (_, phis)) = Map.lookup reg (bPhis b)
  let block = b { bPhis = Map.insert reg (t, phiVal:phis) (bPhis b)}
  let newFunction = f { fBlocks = Map.insert label block (fBlocks f) }
  setCurrentFn newFunction 
  return ()

clearNewProgram :: OM ()
clearNewProgram = do
  -- todo
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
  let newProgram = sNewProgram state
  return newProgram

-- assign register to block -- 
writeVariable :: Reg -> Label -> Reg -> OM ()
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

-- read register from block --
readVariable :: Reg -> Label -> OM Reg
readVariable reg label = do
  state <- get
  let currentDef = sCurrentDef state
  case Map.lookup reg currentDef of
    (Just innerMap) -> do
      case Map.lookup label innerMap of
        (Just val) ->
          return val
        Nothing ->
          readVariableRecursive reg label
    Nothing -> do
      readVariableRecursive reg label

-- read register recursively through predecessor blocks --
readVariableRecursive :: Reg -> Label -> OM Reg
readVariableRecursive reg label = do
  block <- getBlock label
  val <- case bInBlocks block of 
    [inBlockLabel] -> do -- single predecessor, no phi
      readVariable reg label
    inBlockLabels -> do
      readVariable reg label -- todo usunac to i dodac z papera
  writeVariable reg label val
  return val
