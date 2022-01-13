module Backend.Run where

import Latte.Abs
import Backend.Environment
import Backend.CompileLLVM
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

-- run Compiler Monad --
runCM :: CM a -> Store -> Env -> IO (Either String a, Store)
runCM m st en = runStateT (runExceptT (runReaderT m en)) st

-- start compiling program from main -- 
runCompilation :: Program -> IO (Either String String, Store)
runCompilation program = 
  runCM (runMain program) initStore initEnv

