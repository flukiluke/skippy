
module SemanticErrors where

data SemanticError
    = DuplicateDefinition Int Int
    | ArrayTooSmall Int Int
    | BadArrayType Int Int
    | BadVariableType Int Int
    | UnimplementedFeature Int Int

instance Show SemanticError where
    show (DuplicateDefinition _ _) = "Duplicate definition"
    show (ArrayTooSmall _ _) = "Array must have size > 0"
    show (BadArrayType _ _) = "Array type must be integer, boolean or a record"
    show (BadVariableType _ _) = "Unknown type for variable"
    show (UnimplementedFeature _ _) = "Unimplemented feature"
