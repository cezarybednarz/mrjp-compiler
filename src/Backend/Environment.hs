module Backend.Environment where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe
import Data.Map as Map
import Backend.LLVM
import Latte.Abs

-- current free register value --
newtype CurrReg = Reg Integer

-- currently used variables --
newtype Regs = Regs (Map.Map String Reg) 
  deriving (Show)

-- environment --
type Scope = Int
type Loc = Int

type ValEnv = Map.Map Ident (Loc, Scope)
type FuncEnv = Map.Map Ident Loc
type FnRetVal = Val

-- environment monad --
type Env = (ValEnv, FuncEnv, FnRetVal, Scope)

-- store monad -- 
type Store = Map.Map Loc Val

-- compiler monad --
type CM a = ReaderT Env (ExceptT String (StateT Store IO)) a

-- initial environment --
initStore :: Store
initStore = Map.empty

-- todo zmienic VUndef na cos odpowiedniego
initEnv :: Env
initEnv = (Map.empty, Map.empty, VUndef, 0)

