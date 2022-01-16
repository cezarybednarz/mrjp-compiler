module Backend.Environment where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import Data.Map as Map
import Backend.LLVM as LLVM
import Latte.Abs as Latte

-- environment --
type Scope = Int

type ValEnv = Map.Map Ident (Reg, Scope)
type FnRetReg = Reg

-- environment monad --
data Env = Env {eValEnv :: ValEnv,
            eScope :: Scope }
  deriving (Show)

-- compiler state monad -- 
type Functions = [Fn] 
type StrConstants = Map.Map Int StrConstant

data CompilerState = CompilerState { sStrConstants :: StrConstants,
                                     sFunctions :: Functions, 
                                     sCurrLabel :: Label, 
                                     sCurrReg :: Reg }
  deriving (Show)

-- RetInfo --
data RetInfo = Return (LLVM.Type, Reg)
             | ReturnNothing
  deriving (Show, Eq, Ord)
  
-- compiler monad --
type CM a = ReaderT Env (ExceptT String (StateT CompilerState IO)) a

-- initial environment --
initCompilerState :: CompilerState
initCompilerState = CompilerState {
  sStrConstants = Map.empty,
  sFunctions = [],
  sCurrLabel = Label 0,
  sCurrReg = Reg 0 }

-- todo zmienic Reg 0 na cos odpowiedniego
initEnv :: Env
initEnv = Env {
  eValEnv = Map.empty,
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


