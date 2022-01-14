module Backend.Environment where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import Data.Map as Map
import Backend.LLVM as LLVM
import Latte.Abs

-- currently used variables --
newtype Regs = Regs (Map.Map String Reg) 
  deriving (Show)

-- environment --
type Scope = Int
type Loc = Int

type ValEnv = Map.Map Ident (Loc, Scope)
type FuncEnv = Map.Map Ident Loc
type FnRetReg = Reg

-- environment monad --
data Env = Env {eValEnv :: ValEnv, 
            eFuncEnv :: FuncEnv,
            eScope :: Scope }
  deriving (Show)

-- compiler state monad -- 
type Store = Map.Map Loc Reg
type Functions = [Fn]
type CurrLabel = Int -- current free label
type CurrReg = Int   -- current free register

data CompilerState = CompilerState { sStore :: Store, 
                                     sFunctions :: Functions, 
                                     sCurrLabel :: CurrLabel, 
                                     sCurrReg :: CurrReg }
  deriving (Show)

-- RetInfo --
data RetInfo = Return (LLVM.Type, Val)
             | ReturnNothing

-- compiler monad --
type CM a = ReaderT Env (ExceptT String (StateT CompilerState IO)) a

-- initial environment --
initCompilerState :: CompilerState
initCompilerState = CompilerState {
  sStore = Map.empty,
  sFunctions = [],
  sCurrLabel = 0,
  sCurrReg = 0 }

-- todo zmienic Reg 0 na cos odpowiedniego
initEnv :: Env
initEnv = Env {
  eValEnv = Map.empty,
  eFuncEnv = Map.empty, 
  eScope = 0}

-- ! debug functions

debugEnv :: CM ()
debugEnv = do
  env <- ask
  liftIO $ print env

debugState :: CM ()
debugState = do
  state <- get
  liftIO $ print state


