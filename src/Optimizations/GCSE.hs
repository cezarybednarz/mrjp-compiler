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
  sRValMap :: Map.Map LLVMStmt (Label, Reg),
  sDominatorTree :: Map.Map Label (Set.Set Label)
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

clearRValMap :: OM ()
clearRValMap = do
  state <- get
  put $ state { sRValMap = Map.empty }

clearDominatorTree :: OM ()
clearDominatorTree = do
  state <- get
  put $ state { sDominatorTree = Map.empty }

-- optimizer monad -- 
type OM a = (StateT OptimizerState IO) a

-- initial state 
initOptimizerState :: LLVMProgram -> OptimizerState
initOptimizerState llvmProgram = OptimizerState {
  sNewProgram = llvmProgram,
  sCurrentFn = Nothing,
  sRValMap = Map.empty,
  sDominatorTree = Map.empty
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
  buildDominatorTree

  debugState <- get
  debugString $ show $ sDominatorTree debugState
  -- todo optimizeBlocks
  getCurrentFn

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