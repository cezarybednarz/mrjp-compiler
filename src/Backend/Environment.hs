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

type ValEnv = Map.Map Ident (LLVM.Type, Reg, Scope)
type FnRetReg = Reg

-- environment monad --
data Env = Env {eValEnv :: ValEnv,
            eScope :: Scope }
  deriving (Show)

-- compiler state monad -- 
type Functions = [Fn] 
type StrConstants = Map.Map Int StrConstant
type FunctionTypes = Map.Map String (LLVM.Type, [LLVM.Type])

data CompilerState = CompilerState { sStrConstants :: StrConstants,
                                     sFunctions :: Functions, 
                                     sCurrLabel :: Label, 
                                     sCurrReg :: Reg ,
                                     sFunctionTypes :: FunctionTypes}
  deriving (Show)

-- RetInfo --
data RetInfo = Return (LLVM.Type, Val)
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
  sCurrReg = Reg 0 ,
  sFunctionTypes = Map.empty
  }

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

debugString :: String -> CM ()
debugString str = do
  liftIO $ print str


