module Frontend.Environment where

import Latte.Abs
import Data.Map as Map 
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe

-- Enviroment --

type Loc = Int

type ValEnv = Map.Map Ident Loc

type FuncEnv = Map.Map Ident Func

type Env = (ValEnv, FuncEnv)

type Store = Map.Map Loc Val


-- Semantic Analysis Monad --

type SAM a = ReaderT Env (ExceptT String (StateT Store IO)) a

-- Latte variables --

data Func = VFunc Type Ident [Arg] Block

data Val
    = VInt
    | VBool
    | VString
    | VVoid
  deriving (Eq, Ord)


instance Show Func where
  show (VFunc _ id _ _) = show id

instance Show Val where
  show VInt = show "VInt"
  show VBool = show "VBool"
  show VString = show "VString"
  show VVoid = show "VVoid"

-- RetInfo --

data RetInfo = Return Val
             | ReturnNothing

-- Init Enviroment --

initStore :: Store
initStore = Map.empty

initEnv :: Env
initEnv = (Map.empty, Map.empty)

-- !debug --

debug :: SAM ()
debug = do
  (m1, m2) <- ask
  store <- get
  liftIO $ print $ "vars:  " ++ show m1
  liftIO $ print $ "funcs: " ++ show m2
  liftIO $ print $ "store: " ++ show store
  liftIO $ print "---------"
