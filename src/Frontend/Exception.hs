module Frontend.Exception where

import Latte.Abs

errMessage :: BNFC'Position -> SemanticAnalysisException -> String
errMessage line exception =
  case line of
    Just (line, col) -> "ERROR: line " ++ show line ++ " column " ++ show col ++ ": " ++ show exception
    Nothing -> "ERROR: " ++ show exception

data SemanticAnalysisException
    = AssignmentMismatch
    | Redeclared Ident
    | TypeMismatch Type Type
    | Undefined Ident
    | InvalidIndirect Type
    | InvalidOperation
    | ReturnTooMany
    | ReturnNotEnough
    | MissingReturn
    | NonBoolCondition Type
    | FuncNotCalled
    | NonRetFuncAsValue
    | CallNonFunction Ident Type
    | UntypedNil
    | MainUndeclared
    | MainWrongType

instance Show SemanticAnalysisException where
  show MainUndeclared = show "main() undeclared"

