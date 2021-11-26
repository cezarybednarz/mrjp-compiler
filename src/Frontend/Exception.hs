module Frontend.Exception where

import Frontend.Environment
import Latte.Abs
import Control.Monad.Except

errMessage :: BNFC'Position -> SemanticAnalysisException -> String
errMessage line exception =
  case line of
    Just (line, col) -> 
      "ERROR: line " ++ show line ++ " column " ++ show col ++ ": " ++ show exception
    Nothing -> 
      "ERROR: " ++ show exception

data SemanticAnalysisException
    = MainUndeclared
    | FunctionUndeclared Ident
    | VariableUndeclared Ident
    | FuncArgsNumberMismatch Ident
    | ArithmOpTypeMismatch
    | BoolOpTypeMismatch
    | NonIntArgument
    | NonBoolArgument
    | FuncWrongValueReturned Ident Val
    | FuncNoValueReturned Ident
    | DeclTypeMismatch Ident

instance Show SemanticAnalysisException where
  show MainUndeclared = show "main() undeclared"
  show (FunctionUndeclared ident) = show $ "function " ++ show ident ++ " undeclared"
  show (VariableUndeclared ident) = show $ "variable " ++ show ident ++ " undeclared"
  show (FuncArgsNumberMismatch ident) = show $ "function " ++ show ident ++ " called with wrong number of arguments"
  show ArithmOpTypeMismatch = show "values should be integers in integer operator"
  show BoolOpTypeMismatch = show "values should be boolean in boolean operator"
  show NonIntArgument = show "argument should be integer"
  show NonBoolArgument = show "argument should be boolean"
  show (FuncWrongValueReturned ident val) = show $ "function " ++ show ident ++ " should return " ++ show val
  show (FuncNoValueReturned ident) = show $ "function " ++ show ident ++ " should return some value"
  show (DeclTypeMismatch ident) = show $ "variable " ++ show ident ++ " should be defined with different type"






