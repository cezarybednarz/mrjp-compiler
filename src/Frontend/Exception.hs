module Frontend.Exception where

import Latte.Abs

throwErrMessage :: BNFC'Position -> SemanticAnalysisException -> SAM ()
throwErrMessage line exception =
  case line of
    Just (line, col) -> throwError $ "ERROR: line " ++ show line ++ " column " ++ show col ++ ": " ++ show exception
    Nothing -> throwError $ "ERROR: " ++ show exception

data SemanticAnalysisException
    = MainUndeclared
    | FunctionUndeclared Ident
    | VariableUndeclared Ident
    | FuncArgsNumberMismatch Ident
    | ArithmOpTypeMismatch
    | BoolOpTypeMismatch
    | Integer -- todo

instance Show SemanticAnalysisException where
  show MainUndeclared = show "main() undeclared"
  show (FunctionUndeclard Ident) = show "function " ++ ident ++ " undeclared"
  show (VariableUndeclared Ident) = show "variable " ++ ident ++ " undeclared"
  show (FuncArgsNumberMismatch Ident) = show "function " ++ ident ++ " called with wrong number of arguments"
  show ArithmOpTypeMismatch = show "values should be integers in integer operator"
  show BoolOpTypeMismatch = show "values should be boolean in boolean operator"



