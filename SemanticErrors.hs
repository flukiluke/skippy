
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
    show (DuplicateDefinition _) = "Duplicate definition"
    show (ArrayTooSmall _) = "Array must have size > 0"
    show (BadArrayType _) = "Array type must be integer, boolean or a record"
    show (BadVariableType _) = "Unknown type for variable"
    show (UndeclaredSymbol _ name) = name ++ " is not declared"
    show (TypeMismatch _ expected actual) = "Value was of type " ++ actual ++ " but expected " ++ expected
    show (NotReference _) = "Argument is not of the correct form to be passed by reference (did you forget a 'val'?)"
    show (UnexpectedField _) = "Type is not a record type"
    show (UnimplementedFeature _) = "Unimplemented feature"
