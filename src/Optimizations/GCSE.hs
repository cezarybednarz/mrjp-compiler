module Optimizations.GCSE (runGCSE) where

import Latte.Abs as Latte
import Backend.LLVM as LLVM
import Control.Monad.State
import Data.Map as Map
import Data.Set as Set
import Data.Foldable

-- GCSE algorithm based on dominator tree -- 
-- this algorithm also executes LCSE in basic blocks --

data OptimizerState = OptimizerState {
  sNewProgram :: LLVMProgram,
  sCurrentFn :: Maybe Fn,
  sRValMap :: Map.Map LLVMStmt (Label, Reg), -- RValues of expressions
  sDominatorTree :: Map.Map Label (Set.Set Label),
  sCurrentDef :: Map.Map Reg Reg -- values to assign optimized registers
}
  deriving (Show, Eq, Ord)

-- acessors to state -- 
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

getBlockPreds :: Label -> OM [Label]
getBlockPreds label = do
  block <- getBlock label
  return $ bInBlocks block

getDominators :: Label -> OM (Set.Set Label)
getDominators label = do
  state <- get
  let dominatorTree = sDominatorTree state
  let (Just dominators) = Map.lookup label dominatorTree
  return dominators

setDominators :: Label -> Set.Set Label -> OM ()
setDominators label dominators = do
  state <- get
  let dominatorTree = sDominatorTree state
  put $ state { sDominatorTree = Map.insert label dominators dominatorTree}

unionDominators :: Label -> Label -> OM (Set.Set Label)
unionDominators label1 label2 = do
  dominators1 <- getDominators label1
  dominators2 <- getDominators label2
  return $ Set.union dominators1 dominators2

intersectionDominators :: Label -> Label -> OM (Set.Set Label)
intersectionDominators label1 label2 = do
  dominators1 <- getDominators label1
  dominators2 <- getDominators label2
  return $ Set.intersection dominators1 dominators2

checkDomination :: Label -> Label -> OM Bool
checkDomination labelSon labelParent = do
  dominators <- getDominators labelSon
  return $ Set.member labelParent dominators

putRValMap :: LLVMStmt -> (Label, Reg) -> OM ()
putRValMap llvmstmt labelReg = do
  state <- get
  let rValMap = sRValMap state
  put $ state { sRValMap = Map.insert llvmstmt labelReg rValMap}
  return ()

getRValMap :: LLVMStmt -> OM (Maybe (Label, Reg))
getRValMap llvmstmt = do
  state <- get
  let rValMap = sRValMap state
  return $ Map.lookup llvmstmt rValMap

clearRValMap :: OM ()
clearRValMap = do
  state <- get
  put $ state { sRValMap = Map.empty }

clearCurrentDefMap :: OM ()
clearCurrentDefMap = do
  state <- get
  put $ state { sCurrentDef = Map.empty }

clearDominatorTree :: OM ()
clearDominatorTree = do
  state <- get
  put $ state { sDominatorTree = Map.empty }

assignReg :: Reg -> Reg -> OM () 
assignReg reg1 reg2 = do
  state <- get
  let currentDef = sCurrentDef state
  let newCurrentDef = Map.insert reg1 reg2 currentDef
  put $ state { sCurrentDef = newCurrentDef }

getReg :: Reg -> OM (Maybe Reg)
getReg reg = do
  state <- get
  let currentDef = sCurrentDef state
  return $ Map.lookup reg currentDef 

getVal :: Val -> OM Val
getVal val = do
  case val of 
    (VReg reg) -> do
      reg' <- getReg reg
      case reg' of
        Nothing -> return (VReg reg)
        (Just r') -> return (VReg r')
    v' -> return v'

-- try finding already defined RValue of stmt in previous dominating block --
-- returns true if stmt should be emmited --
tryOptimize :: (Label, Reg) -> LLVMStmt -> OM Bool
tryOptimize (label, reg) stmt = do
  rValLocation <- getRValMap stmt
  case rValLocation of 
    Nothing -> do
      putRValMap stmt (label, reg)
      return True
    (Just (label', reg')) -> do
      dominating <- checkDomination label label'
      if dominating then do
        assignReg reg reg'
        return False
      else do
        putRValMap stmt (label, reg)
        return True

-- optimizer monad -- 
type OM a = (StateT OptimizerState IO) a

-- initial state 
initOptimizerState :: LLVMProgram -> OptimizerState
initOptimizerState llvmProgram = OptimizerState {
  sNewProgram = llvmProgram,
  sCurrentFn = Nothing,
  sRValMap = Map.empty,
  sDominatorTree = Map.empty,
  sCurrentDef = Map.empty
}

-- run optimizer monad -- 
runOM :: OM a -> OptimizerState -> IO (a, OptimizerState)
runOM = runStateT

-- start optimizing program (entrypoint) --
runGCSE :: LLVMProgram -> IO (LLVMProgram, OptimizerState)
runGCSE llvmProgram =
  runOM runOptimization (initOptimizerState llvmProgram)

-- run optimization -- 
runOptimization :: OM LLVMProgram
runOptimization = do
  optimizeProgram
  getNewProgram

-- run gcse --
optimizeProgram :: OM ()
optimizeProgram = do
  program <- getNewProgram
  let fns = pFunctions program
  newFns <- mapM optimizeFn fns
  setNewProgram program { pFunctions = newFns }

optimizeFn :: Fn -> OM Fn
optimizeFn fn = do
  setCurrentFn fn
  clearDominatorTree
  clearRValMap
  clearCurrentDefMap
  buildDominatorTree
  optimizeBlocks
  getCurrentFn

optimizeBlocks :: OM ()
optimizeBlocks = do
  fn <- getCurrentFn
  let (labels, blocks) = Prelude.unzip $ Map.toList (fBlocks fn)
  newBlocks <- mapM optimizeBlock blocks
  setCurrentFn (fn {fBlocks = Map.fromList (Prelude.zip labels newBlocks)})

optimizeBlock :: LLBlock -> OM LLBlock
optimizeBlock block = do
  let stmts = bStmts block
  let label = bLabel block
  newStmts <- optimizeBlockStmts label stmts
  return $ block {bStmts = newStmts}

optimizeBlockStmts :: Label -> [LLVMStmt] -> OM [LLVMStmt]
optimizeBlockStmts _ [] = return []
optimizeBlockStmts label (stmt:stmts) = do
  newStmt <- optimizeStmt label stmt
  finalStmts <- optimizeBlockStmts label stmts
  case newStmt of 
    (Just stmt') -> do
      return $ stmt' : finalStmts
    Nothing ->
      return finalStmts

translateArgs :: [(LLVM.Type, Val)] -> OM [(LLVM.Type, Val)]
translateArgs args = do
  mapM (\(t, v) -> do
    v' <- getVal v 
    return (t, v') ) 
    args

optimizeStmt :: Label -> LLVMStmt -> OM (Maybe LLVMStmt)
optimizeStmt label llvmstmt = do
  let zeroR = Reg 0
  case llvmstmt of 
    (Call r1 t1 s1 ts) -> do
      ts' <- translateArgs ts
      toAdd <- tryOptimize (label, r1) (Call zeroR t1 s1 ts') 
      if toAdd then
        return $ Just (Call r1 t1 s1 ts')
      else
        return Nothing
    (CallVoid t1 s1 ts) -> do
      ts' <- translateArgs ts
      return $ Just (CallVoid t1 s1 ts')
    (LLVM.Ret t1 v1) -> do
      v1' <- getVal v1
      return $ Just (LLVM.Ret t1 v1')
    (Arithm r1 t1 v1 v2 op) -> do
      v1' <- getVal v1
      v2' <- getVal v2
      toAdd <- tryOptimize (label, r1) (Arithm zeroR t1 v1' v2' op) 
      if toAdd then
        return $ Just (Arithm r1 t1 v1' v2' op)
      else
        return Nothing
    (Br l1) -> return $ Just (Br l1)
    (BrCond t1 v1 l1 l2) -> do
      v1' <- getVal v1
      return $ Just (BrCond t1 v1' l1 l2)
    (Cmp r1 c1 t1 v1 v2) -> do
      v1' <- getVal v1
      v2' <- getVal v2
      toAdd <- tryOptimize (label, r1) (Cmp zeroR c1 t1 v1' v2') 
      if toAdd then
        return $ Just (Cmp r1 c1 t1 v1' v2')
      else
        return Nothing
    (Xor r1 t1 v1 v2) -> do 
      v1' <- getVal v1
      v2' <- getVal v2
      toAdd <- tryOptimize (label, r1) (Xor zeroR t1 v1' v2')
      if toAdd then
        return $ Just (Xor r1 t1 v1' v2')
      else 
        return Nothing

-- build dominator tree from blocks -- 
buildDominatorTree :: OM ()
buildDominatorTree = do
  fn <- getCurrentFn
  let (firstBlock:blocks, _) = Prelude.unzip $ Map.toList (fBlocks fn)
  setDominators firstBlock (Set.fromList [firstBlock])
  mapM_ (\block -> setDominators block (Set.fromList (firstBlock:blocks))) blocks
  buildDominatorLoop blocks

buildDominatorLoop :: [Label] -> OM ()
buildDominatorLoop labels = do
  changed <- buildDominatorWhileChangesLoop labels
  when changed $ buildDominatorLoop labels

buildDominatorWhileChangesLoop :: [Label] -> OM Bool
buildDominatorWhileChangesLoop [] = return False
buildDominatorWhileChangesLoop (label:labels) = do
  preds <- getBlockPreds label
  predsDominators <- mapM getDominators preds
  let newDominators = Set.union (Set.fromList [label]) (Prelude.foldr Set.intersection (head predsDominators) predsDominators)
  dominators <- getDominators label
  if dominators == newDominators then do 
    buildDominatorWhileChangesLoop labels
  else do
    setDominators label newDominators
    buildDominatorWhileChangesLoop labels
    return True

-- ! debug functions

debugState :: OM ()
debugState = do
  state <- get
  liftIO $ print state

debugString :: String -> OM ()
debugString str = do
  liftIO $ print str