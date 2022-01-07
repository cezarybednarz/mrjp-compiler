module Frontend.Environment where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map             as Map
import           Data.Maybe
import           Latte.Abs

-- enviroment --

type Scope = Int
type Loc = Int

type ValEnv = Map.Map Ident (Loc, Scope)
type FuncEnv = Map.Map Ident Func
type FnRetVal = Val
type Env = (ValEnv, FuncEnv, FnRetVal, Scope)

type Store = Map.Map Loc Val

-- tuple selection for Env --
fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

thrd4 :: (a, b, c, d) -> c
thrd4 (_, _, x, _) = x

frth4 :: (a, b, c, d) -> d
frth4 (_, _, _, x) = x

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
  show VInt    = show "Int"
  show VBool   = show "Bool"
  show VString = show "String"
  show VVoid   = show "Void"

-- RetInfo --

data RetInfo = Return Val
             | ReturnNothing

-- init enviroment --

initStore :: Store
initStore = Map.empty

initEnv :: Env
initEnv = (Map.empty, Map.empty, VInt, 0)

-- !debug --

debug :: SAM ()
debug = do
  (m1, m2, fnRetVal, scope) <- ask
  store <- get
  liftIO $ print " "
  liftIO $ print $ "vars:     " ++ show m1
  liftIO $ print $ "funcs:    " ++ show m2 
  liftIO $ print $ "fnRetVal: " ++ show fnRetVal
  liftIO $ print $ "scope:    " ++ show scope
  liftIO $ print "---------"
  liftIO $ print $ "store:    " ++ show store
  liftIO $ print "---------\n"

debugStr :: String -> SAM ()
debugStr str =
  liftIO $ print str
