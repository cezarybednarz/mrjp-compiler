module Backend.Run where

import Latte.Abs
import Backend.Environment
import Backend.CompileLLVM
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

-- run Compiler Monad --
runCM :: CM a -> CompilerState -> Env -> IO (Either String a, CompilerState)
runCM m cs en = runStateT (runExceptT (runReaderT m en)) cs

-- start compiling program from main -- 
runCompilation :: Program -> IO (Either String String, CompilerState)
runCompilation program = 
  runCM (runMain program) initCompilerState initEnv

