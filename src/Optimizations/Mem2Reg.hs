
module Optimizations.Mem2Reg (runMem2Reg, runTrivialPhiReduction) where

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
  sCurrentDef :: Map.Map Reg (Map.Map Label TypeVal),
  sTransPhis :: Map.Map Reg Val -- translate phi values after trivial phi reduction
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

setRegister :: Reg -> OM ()
setRegister reg = do
  program <- getNewProgram
  setNewProgram $ program { pCurrReg = reg }

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

setBlock :: Label -> LLBlock -> OM ()
setBlock label block = do
  fn <- getCurrentFn
  let newBlocks = Map.insert label block (fBlocks fn)
  setCurrentFn $ fn { fBlocks = newBlocks }
  return ()

getBlockPhis :: Label -> OM (Map.Map Reg (LLVM.Type, [(Val, Label)]))
getBlockPhis label = do
  block <- getBlock label
  return $ bPhis block

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

popPhiForBlock :: Label -> LLVM.Type -> Reg -> OM ()
popPhiForBlock label t reg = do
  b <- getBlock label
  f <- getCurrentFn
  state <- get
  let (Just (_, phi:phis)) = Map.lookup reg (bPhis b)
  let block = b { bPhis = Map.insert reg (t, phis) (bPhis b)}
  let newFunction = f { fBlocks = Map.insert label block (fBlocks f) }
  setCurrentFn newFunction
  return ()

removePhiForBlock :: Label -> Reg -> OM ()
removePhiForBlock label reg = do
  b <- getBlock label
  f <- getCurrentFn
  state <- get
  let block = b { bPhis = Map.delete reg (bPhis b)}
  let newFunction = f { fBlocks = Map.insert label block (fBlocks f) }
  setCurrentFn newFunction
  return ()

clearCurrentDef :: OM ()
clearCurrentDef = do
  state <- get
  put $ state { sCurrentDef = Map.empty }

putArgsInFirstBlock :: Reg -> [(LLVM.Type, String)] -> OM ()
putArgsInFirstBlock _ [] = return ()
putArgsInFirstBlock (Reg r) ((t, _):args) = do
  fn <- getCurrentFn
  writeVariable (Reg r) (Label 0) (TypeVal t (VReg (Reg r)))
  putArgsInFirstBlock (Reg (r + 1)) args

getTransPhiVal :: Reg -> OM (Maybe Val)
getTransPhiVal reg = do
  gets (Map.lookup reg . sTransPhis)

setTransPhiVal :: Reg -> Val -> OM ()
setTransPhiVal reg val = do
  state <- get
  put $ state { sTransPhis = Map.insert reg val (sTransPhis state)}

-- optimizer monad -- 
type OM a = (StateT OptimizerState IO) a

-- initial state 
initOptimizerState :: LLVMProgram -> OptimizerState
initOptimizerState llvmProgram = OptimizerState {
  sNewProgram = llvmProgram,
  sCurrentFn = Nothing,
  sCurrentDef = Map.empty,
  sTransPhis = Map.empty
}

-- run optimizer monad -- 
runOM :: OM a -> OptimizerState -> IO (a, OptimizerState)
runOM = runStateT

-- start optimizing program (entrypoint) --
runMem2Reg :: LLVMProgram -> IO (LLVMProgram, OptimizerState)
runMem2Reg llvmProgram =
  runOM runOptimization (initOptimizerState llvmProgram)

-- start reduction of trivial phis (entrypoint) -- 
runTrivialPhiReduction :: LLVMProgram -> IO (LLVMProgram, OptimizerState)
runTrivialPhiReduction llvmProgram =
  runOM runPhiReduction (initOptimizerState llvmProgram)

-- run optimization -- 
runOptimization :: OM LLVMProgram
runOptimization = do
  newProgram <- getNewProgram
  optimizedFunctions <- optimizeFns (pFunctions newProgram)
  setNewProgram (newProgram { pFunctions = optimizedFunctions })
  gets sNewProgram

-- run trivial phi reduction --
runPhiReduction :: OM LLVMProgram
runPhiReduction = do
  reducePhiProgram
  getNewProgram

-- trivial phi reduction functions --
reducePhiProgram :: OM ()
reducePhiProgram = do
  program <- getNewProgram
  let fns = pFunctions program
  newFns <- mapM reducePhiFn fns
  setNewProgram program { pFunctions = newFns}

reducePhiFn :: Fn -> OM Fn
reducePhiFn fn = do
  setCurrentFn fn
  reducePhiFnLoop
  getCurrentFn

reducePhiFnLoop :: OM ()
reducePhiFnLoop = do
  fn <- getCurrentFn
  let (labels, _) = Prelude.unzip $ Map.toList (fBlocks fn)
  trivialPhi <- findTrivialPhiForBlocks labels
  case trivialPhi of 
    Nothing -> return ()
    (Just phi) -> do
      translateValuesForBlocks phi labels
      reducePhiFnLoop

findTrivialPhiForBlocks :: [Label] -> OM (Maybe (Reg, Val))
findTrivialPhiForBlocks [] = return Nothing
findTrivialPhiForBlocks (label:labels) = do
  block <- getBlock label
  let phis = Map.toList (bPhis block)
  trivialPhi <- findTrivialPhis phis
  case trivialPhi of 
    Nothing -> 
      findTrivialPhiForBlocks labels
    (Just (reg, val)) -> do
      removePhiForBlock label reg
      return $ Just (reg, val)

findTrivialPhis :: [(Reg, (LLVM.Type, [(Val, Label)]))] -> OM (Maybe (Reg, Val))
findTrivialPhis [] = return Nothing
findTrivialPhis ((reg, (t, [(v1, l1), (v2, l2)])):phis) = do
  if v1 == v2 then 
    return $ Just (reg, v1)
  else
    findTrivialPhis phis

translateValuesForBlocks :: (Reg, Val) -> [Label] -> OM ()
translateValuesForBlocks _ [] = return ()
translateValuesForBlocks regVal (label:labels) = do
  translateStmtsForBlock regVal label
  translatePhisForBlock regVal label
  translateValuesForBlocks regVal labels

translateStmtsForBlock :: (Reg, Val) -> Label -> OM ()
translateStmtsForBlock regVal label = do
  block <- getBlock label
  let stmts = bStmts block
  newStmts <- mapM (translateStmt regVal) stmts
  let newBlock = block { bStmts = newStmts}
  setBlock label newBlock
  return ()

translateArgs :: [(LLVM.Type, Val)] -> (Reg, Val) -> OM [(LLVM.Type, Val)]
translateArgs args regVal= do
  mapM (\(t, v) -> do
    v' <- transVal v regVal
    return (t, v') ) 
    args

translateStmt :: (Reg, Val) -> LLVMStmt -> OM LLVMStmt
translateStmt regVal llvmstmt = do
  case llvmstmt of 
    (Call r1 t1 s1 ts) -> do
      newTs <- translateArgs ts regVal
      return (Call r1 t1 s1 newTs)
    (CallVoid t1 s1 ts) -> do
      newTs <- translateArgs ts regVal
      return (CallVoid t1 s1 newTs)
    (LLVM.Ret t1 v1) -> do
      v1' <- transVal v1 regVal
      return (LLVM.Ret t1 v1')
    RetVoid -> do return RetVoid
    (Arithm r1 t1 v1 v2 op) -> do
      v1' <- transVal v1 regVal
      v2' <- transVal v2 regVal
      return (Arithm r1 t1 v1' v2' op)
    (Br l1) -> return (Br l1)
    (BrCond t1 v1 l1 l2) -> do
      v1' <- transVal v1 regVal
      return (BrCond t1 v1' l1 l2)
    (Cmp r1 c1 t1 v1 v2) -> do
      v1' <- transVal v1 regVal
      v2' <- transVal v2 regVal
      return (Cmp r1 c1 t1 v1' v2')
    (Xor r1 t1 v1 v2) -> do 
      v1' <- transVal v1 regVal
      v2' <- transVal v2 regVal
      return (Xor r1 t1 v1' v2')

translatePhisForBlock :: (Reg, Val) -> Label -> OM ()
translatePhisForBlock regVal label = do
  block <- getBlock label
  let (regs, phisVal) = Prelude.unzip $ Map.toList (bPhis block)
  newPhisVal <- mapM (\(t, [(v1, l1), (v2, l2)]) -> do
      v1' <- transVal v1 regVal
      v2' <- transVal v2 regVal
      return  (t, [(v1', l1), (v2', l2)])
    ) phisVal
  let newBlock = block { bPhis = Map.fromList (Prelude.zip regs newPhisVal) }
  setBlock label newBlock
  return ()

transVal :: Val -> (Reg, Val) -> OM Val
transVal val (reg, phiVal) = do
  case val of 
    (VReg r) -> 
      if r == reg then
        return phiVal
      else
        return (VReg r)
    v' -> return v'

-- functions for optimizing structures in LLVM --
optimizeStmt :: Label -> LLVMStmt -> OM (Maybe LLVMStmt)
optimizeStmt label llvmstmt = do
  case llvmstmt of
    (Call r1 t1 s1 ts) -> do
      ts' <- optimizeArgsList label ts
      writeVariable r1 label (TypeVal t1 (VReg r1))
      return $ Just (Call r1 t1 s1 ts')
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
      v1' <- readVal (TypeVal t1 v1) label
      writeVariable r label (TypeVal t1 v1')
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
  return $ (t, val') : optimizedArgsList

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
optimizeBackendPhis :: Label -> OM ()
optimizeBackendPhis label = do
  block <- getBlock label
  let (phiRegs, phiVals) = Prelude.unzip (Map.toList (bPhis block))
  let (phiTypes, _) = Prelude.unzip phiVals
  let phis = Prelude.zip phiTypes phiRegs
  mapM_ (\(t, r) -> do
    --r' <- readVal (TypeVal t (VReg r)) label
    writeVariable r label (TypeVal t (VReg r)))  -- todo sprawdzic czy na pewno r'
    phis

-- fill in empty phis generated by algorithm -- 
optimizeEmptyPhi :: Label -> (Reg, (LLVM.Type, [(Val, Label)])) -> OM ()
optimizeEmptyPhi label (variable, (t, vals)) = do
  case vals of
    [(VReg val, _)] -> do
      popPhiForBlock label t variable
      addPhiOperands val label (TypeVal t (VReg variable))
      return ()
    _ -> do
      return ()

optimizeEmptyPhis :: Label -> OM ()
optimizeEmptyPhis label = do
  block <- getBlock label
  let phis = Map.toList $ bPhis block
  mapM_ (optimizeEmptyPhi label) phis

optimizeBlock :: Label -> OM ()
optimizeBlock label = do
  block <- getBlock label
  let stmts = bStmts block
  optimizeBackendPhis label
  newStmts <- optimizeStmtsList label stmts
  newPhis <- getBlockPhis label
  setBlock label (block { bStmts = newStmts, bPhis = newPhis })

optimizeCurrFn :: OM ()
optimizeCurrFn = do
  clearCurrentDef
  fn <- getCurrentFn
  putArgsInFirstBlock (Reg 0) (fArgs fn)
  setRegister (fMaxRegister fn)
  let (labels, _) = Prelude.unzip $ Map.toList $ fBlocks fn
  mapM_ optimizeBlock labels
  mapM_ optimizeEmptyPhis labels

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
readVariableRecursive t variable label = do
  block <- getBlock label
  val <- case bInBlocks block of
    [inBlockLabel] -> do -- single predecessor, no phi
      readVariable t variable inBlockLabel
    inBlockLabels -> do
      val <- newRegister
      putEmptyPhiForBlock label t val
      writeVariable variable label (TypeVal t (VReg val))
      putPhiForBlock label t val (VReg variable, label) -- in order to reconstruct in later phase
      return (TypeVal t (VReg val))
  writeVariable variable label val
  return val

-- add phi operand (from modern SSA algorithm)--
addPhiOperands :: Reg -> Label -> TypeVal -> OM TypeVal
addPhiOperands variable label (TypeVal t val) = do
  block <- getBlock label
  let preds = bInBlocks block
  mapM_ (appendOperands label variable (TypeVal t val)) preds
  return (TypeVal t val)

-- helper function for addPhiOperands --
appendOperands :: Label -> Reg -> TypeVal -> Label -> OM ()
appendOperands label variable (TypeVal t (VReg val)) label2 = do
  TypeVal _ variable' <- readVariable t variable label2
  putPhiForBlock label t val (variable', label2)

-- ! debug functions 

debugState :: OM ()
debugState = do
  state <- get
  liftIO $ print state

debugString :: String -> OM ()
debugString str = do
  liftIO $ print str


showPhisBlock :: LLBlock -> String
showPhisBlock block = do
  show (bLabel block) ++ ": "
  ++ "  " ++ show (bPhis block)

debugPhisFn :: Fn -> OM ()
debugPhisFn fn = do
  let blocks = fBlocks fn
  mapM_ (liftIO . print . showPhisBlock) blocks