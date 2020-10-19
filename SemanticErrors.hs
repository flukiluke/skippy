
module SemanticErrors where

data SemanticError
    = DuplicateDefinition Int Int
    | ArrayTooSmall Int Int
    | BadArrayType Int Int
    | BadVariableType Int Int
    | UndeclaredSymbol String Int Int
    | TypeMismatch String String Int Int
    | NotReference Int Int
    | UnexpectedField Int Int
    | UnimplementedFeature Int Int

instance Show SemanticError where
    show (DuplicateDefinition _ _) = "Duplicate definition"
    show (ArrayTooSmall _ _) = "Array must have size > 0"
    show (BadArrayType _ _) = "Array type must be integer, boolean or a record"
    show (BadVariableType _ _) = "Unknown type for variable"
    show (UndeclaredSymbol name _ _) = name ++ " is not declared"
    show (TypeMismatch expected actual _ _) = "Value was of type " ++ actual ++ " but expected " ++ expected
    show (NotReference _ _) = "Argument is not of the correct form to be passed by reference (did you forget a 'val'?)"
    show (UnexpectedField _ _) = "Type is not a record type"
    show (UnimplementedFeature _ _) = "Unimplemented feature"
