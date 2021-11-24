module Frontend.Run where

import Latte.Abs
import Frontend.Environment
import Frontend.Analyse
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

-- run Typechecker Monad --

runSAM :: SAM a -> Store -> Env -> IO (Either String a, Store)
runSAM m st en = runStateT (runExceptT (runReaderT m en)) st

-- Start program from main --

runSemanticAnalysis :: Program -> IO (Either String Val, Store)
runSemanticAnalysis program =
  runSAM (runMain program) initStore initEnv




