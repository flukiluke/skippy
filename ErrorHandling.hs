module ErrorHandling where

type Posn = (Int, Int)

data SemanticError
    = DuplicateDefinition { errPosn :: Posn }
    | ArrayTooSmall { errPosn :: Posn }
    | BadIndex { errPosn :: Posn }
    | BadArrayType { errPosn :: Posn }
    | BadVariableType { errPosn :: Posn }
    | UndeclaredSymbol { errPosn :: Posn, errSymbol :: String }
    | TypeMismatch { errPosn :: Posn, errExpected :: String, errActual :: String }
    | NotReference { errPosn :: Posn }
    | UnexpectedField { errPosn :: Posn }
    | NoMainProcedure { errPosn :: Posn }
    | UnimplementedFeature { errPosn :: Posn }

instance Show SemanticError where
    show (DuplicateDefinition posn) = (ref posn) ++ "Duplicate definition"
    show (BadIndex posn) = (ref posn) ++ "Invalid array index"
    show (ArrayTooSmall posn) = (ref posn) ++ "Array must have size > 0"
    show (BadArrayType posn) = (ref posn) ++ "Array type must be integer, boolean or a record"
    show (BadVariableType posn) = (ref posn) ++ "Unknown type for variable"
    show (UndeclaredSymbol posn name) = (ref posn) ++ name ++ " is not declared"
    show (TypeMismatch posn expected actual) = (ref posn) ++ "Value was of type " ++ actual ++ " but expected " ++ expected
    show (NotReference posn) = (ref posn) ++ "Argument is not of the correct form to be passed by reference (did you forget a 'val'?)"
    show (UnexpectedField posn) = (ref posn) ++ "Type is not a record type"
    show (NoMainProcedure posn) = (ref posn) ++ "Program does not contain a procedure main()"
    show (UnimplementedFeature posn) = (ref posn) ++ "Unimplemented feature"

ref (row, col) = (show row) ++ ":" ++ (show col) ++ ": "

-- Get the text on line linenum from content
lineContent :: String -> Int -> String
lineContent content linenum = lines content !! linenum

fancyErrorMessage :: Posn -> String -> String -> String
fancyErrorMessage (-1, -1) _ msg = "At end of input: error: " ++ msg
fancyErrorMessage (row, col) source msg
    = "Line " ++ (show row) ++ " column " ++ (show col) ++ ": error: " ++ msg ++
        "\n\n" ++ (lineContent source (row - 1)) ++
        "\n" ++ (take (col - 1) (repeat ' ')) ++ "^ error here"

semanticError :: SemanticError -> String -> String
semanticError err source
  = fancyErrorMessage (errPosn err) source (show err)
