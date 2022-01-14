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
type Env = (ValEnv, FuncEnv, FnRetReg, Scope)

-- compiler state monad -- 
type Store = Map.Map Loc Reg
type Functions = [Fn]
type CurrLabel = Int -- current free label
type CurrReg = Int   -- current free register

data CompilerState = CompilerState { sStore :: Store, 
                                     sFunctions :: Functions, 
                                     sCurrLabel :: CurrLabel, 
                                     sCurrReg :: CurrReg }

-- RetInfo --
data RetInfo = Return (LLVM.Type, Val)
             | ReturnNothing

-- tuple selection for Env --
fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

thrd4 :: (a, b, c, d) -> c
thrd4 (_, _, x, _) = x

frth4 :: (a, b, c, d) -> d
frth4 (_, _, _, x) = x

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
initEnv = (Map.empty, Map.empty, Reg 0, 0)

