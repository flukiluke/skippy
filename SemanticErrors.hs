
module SemanticErrors where

type Posn = (Int, Int)

data SemanticError
    = DuplicateDefinition Posn
    | ArrayTooSmall Posn
    | BadArrayType Posn
    | BadVariableType Posn
    | UndeclaredSymbol Posn String
    | TypeMismatch Posn String String
    | NotReference Posn
    | UnexpectedField Posn
    | UnimplementedFeature Posn

instance Show SemanticError where
    show (DuplicateDefinition posn) = (ref posn) ++ "Duplicate definition"
    show (ArrayTooSmall posn) = (ref posn) ++ "Array must have size > 0"
    show (BadArrayType posn) = (ref posn) ++ "Array type must be integer, boolean or a record"
    show (BadVariableType posn) = (ref posn) ++ "Unknown type for variable"
    show (UndeclaredSymbol posn name) = (ref posn) ++ name ++ " is not declared"
    show (TypeMismatch posn expected actual) = (ref posn) ++ "Value was of type " ++ actual ++ " but expected " ++ expected
    show (NotReference posn) = (ref posn) ++ "Argument is not of the correct form to be passed by reference (did you forget a 'val'?)"
    show (UnexpectedField posn) = (ref posn) ++ "Type is not a record type"
    show (UnimplementedFeature posn) = (ref posn) ++ "Unimplemented feature"

ref (row, col) = (show row) ++ ":" ++ (show col) ++ ": "
