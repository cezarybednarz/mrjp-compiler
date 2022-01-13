module Frontend.Run where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Frontend.Analyse
import           Frontend.Environment
import           Latte.Abs

-- run Typechecker Monad --
runSAM :: SAM a -> Store -> Env -> IO (Either String a, Store)
runSAM m st en = runStateT (runExceptT (runReaderT m en)) st

-- start program from main --
runSemanticAnalysis :: Program -> IO (Either String (), Store)
runSemanticAnalysis program =
  runSAM (runMain program) initStore initEnv




