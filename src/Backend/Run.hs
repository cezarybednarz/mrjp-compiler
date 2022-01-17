module Backend.Run where

import           Backend.CompileLLVM
import           Backend.Environment
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Latte.Abs

-- run Compiler Monad --
runCM :: CM a -> CompilerState -> Env -> IO (Either String a, CompilerState)
runCM m cs en = runStateT (runExceptT (runReaderT m en)) cs

-- start compiling program from main --
runCompilation :: Program -> IO (Either String String, CompilerState)
runCompilation program =
  runCM (runMain program) initCompilerState initEnv

