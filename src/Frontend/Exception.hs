module Frontend.Exception where

import           Control.Monad.Except
import           Frontend.Environment
import           Latte.Abs
import           Latte.Print

errMessage :: BNFC'Position -> SemanticAnalysisException -> String
errMessage line exception =
  case line of
    Just (line, col) ->
      "ERROR\nline " ++ show line ++ " column " ++ show col ++ ": " ++ show exception
    Nothing ->
      "ERROR\n " ++ show exception

data SemanticAnalysisException
    = FunctionUndeclared Ident
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
    | ArgTypeMismatch Ident Ident
    | MainWrongType
    | MainWrongNumberOfArgs
    | VoidVaribaleDeclaration
    | InvalidStringOperator
    | FunctionRedeclared Ident
    | StringInvalidRel
    | ArrayWrongType 
    | WrongLValue
    -- invalid expression as array name

instance Show SemanticAnalysisException where
  show (FunctionUndeclared (Ident ident)) = "function " ++  show ident ++ " undeclared"
  show (VariableUndeclared (Ident ident)) = "variable " ++ show ident ++ " undeclared"
  show (FuncArgsNumberMismatch (Ident ident)) = "function " ++ show ident ++ " called with wrong number of arguments"
  show (OpTypeMismatch val) = "values should be " ++ show val ++ " in operator"
  show NonIntArgument = "argument should be integer"
  show NonBoolArgument = "argument should be boolean"
  show (FuncWrongValueReturned val) = "function should return " ++ show val
  show (FuncNoValueReturned (Ident ident)) = "function " ++ show ident ++ " should return some value"
  show (DeclTypeMismatch (Ident ident)) = "variable " ++ show ident ++ " should be defined with different type"
  show ConditionNonBoolean = "condition should be boolean"
  show (VariableRedeclared (Ident ident)) = "variable " ++ show ident ++ " redeclared"
  show (AssTypeMismatch (Ident ident)) = "variable " ++ show ident ++ " should be assigned with different type"
  show (ArgTypeMismatch (Ident ident1) (Ident ident2)) = "function " ++ show ident1 ++ " should be called with different type for variable " ++ show ident2
  show MainWrongType = "main should be type 'int'"
  show MainWrongNumberOfArgs = "main shouldn't have any arguments"
  show VoidVaribaleDeclaration = "cannot declare void variable"
  show InvalidStringOperator = "'+' is the only arithmetic operator accepted on strings"
  show (FunctionRedeclared (Ident ident)) = "cannot redeclare function " ++ show ident
  show StringInvalidRel = "'==' and '!=' are the only valid relation operators on strings"
  show ArrayWrongType = "array has wrong type "
  show WrongLValue = "wrong lvalue"






