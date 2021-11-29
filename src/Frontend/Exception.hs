module Frontend.Exception where

import Frontend.Environment
import Latte.Abs
import Control.Monad.Except

errMessage :: BNFC'Position -> SemanticAnalysisException -> String
errMessage line exception =
  case line of
    Just (line, col) -> 
      "ERROR\nline " ++ show line ++ " column " ++ show col ++ ": " ++ show exception
    Nothing -> 
      "ERROR\n " ++ show exception

data SemanticAnalysisException
    = MainUndeclared
    | FunctionUndeclared Ident
    | VariableUndeclared Ident
    | FuncArgsNumberMismatch Ident
    | OpTypeMismatch Val
    | BoolOpTypeMismatch
    | StringOpTypeMismatch
    | NonIntArgument
    | NonBoolArgument
    | FuncWrongValueReturned Val
    | FuncNoValueReturned Ident
    | DeclTypeMismatch Ident
    | ConditionNonBoolean
    | VariableRedeclared Ident 
    | AssTypeMismatch Ident

instance Show SemanticAnalysisException where
  show MainUndeclared = "main() undeclared"
  show (FunctionUndeclared ident) = "function " ++ show ident ++ " undeclared"
  show (VariableUndeclared ident) = "variable " ++ show ident ++ " undeclared"
  show (FuncArgsNumberMismatch ident) = "function " ++ show ident ++ " called with wrong number of arguments"
  show (OpTypeMismatch val) = "values should be " ++ show val ++ " in operator"
  show NonIntArgument = "argument should be integer"
  show NonBoolArgument = "argument should be boolean"
  show (FuncWrongValueReturned val) = "function should return " ++ show val
  show (FuncNoValueReturned ident) = "function " ++ show ident ++ " should return some value"
  show (DeclTypeMismatch ident) = "variable " ++ show ident ++ " should be defined with different type"
  show ConditionNonBoolean = "condition should be boolean" 
  show (VariableRedeclared ident) = "variable " ++ show ident ++ " redeclared"
  show (AssTypeMismatch ident) = "variable " ++ show ident ++ " should be assigned with different value" 






