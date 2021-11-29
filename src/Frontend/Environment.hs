module Frontend.Environment where

import Latte.Abs
import Data.Map as Map 
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe

-- enviroment --

type Loc = Int
type ValEnv = Map.Map Ident Loc
type FuncEnv = Map.Map Ident Func
type FnRetVal = Val
type Env = (ValEnv, FuncEnv, FnRetVal) -- todo
type Store = Map.Map Loc Val

-- tuple selection for Env -- 
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thrd3 :: (a, b, c) -> c
thrd3 (_, _, x) = x


-- semantic analysis monad --

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
  show VInt = show "Int"
  show VBool = show "Bool"
  show VString = show "String"
  show VVoid = show "Void"

-- RetInfo --

data RetInfo = Return Val
             | ReturnNothing

-- init enviroment --

initStore :: Store
initStore = Map.empty

initEnv :: Env
initEnv = (Map.empty, Map.empty, VInt)

-- !debug --

debug :: SAM ()
debug = do
  (m1, m2, fnRetVal) <- ask
  store <- get
  liftIO $ print $ "fnRetVal: " ++ show fnRetVal
  liftIO $ print $ "vars:     " ++ show m1
  liftIO $ print $ "funcs:    " ++ show m2
  liftIO $ print $ "store:    " ++ show store
  liftIO $ print "---------"
