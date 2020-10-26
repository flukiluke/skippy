-- Skippy, a compiler for the Roo language
--
-- Submitted for assignment 3 of COMP90045, 2020
-- By Luke Ceddia [lceddia] and Ben Harper [bharper1]
-- 28 October 2020
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.
--
-- This module provides errors that can be throw during the semantic analysis
-- phase, and functions for printing those errors in a human-friendly way.

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
    show (DuplicateDefinition _)
      = "Duplicate definition"
    show (BadIndex _)
      = "Invalid array index"
    show (ArrayTooSmall _)
      = "Array must have size > 0"
    show (BadArrayType _)
      = "Array type must be integer, boolean or a record"
    show (BadVariableType _)
      = "Unknown type for variable"
    show (UndeclaredSymbol _ name)
      = name ++ " is not declared"
    show (TypeMismatch _ expected actual)
      = "Value was of type " ++ actual ++ " but expected " ++ expected
    show (NotReference _)
      = "Argument is not of the correct form to be passed by reference (did you forget a 'val'?)"
    show (UnexpectedField _)
      = "Type is not a record type"
    show (NoMainProcedure _)
      = "Program does not contain a procedure main()"
    show (UnimplementedFeature _)
      = "Unimplemented feature"

-- Get the text on line linenum from content
lineContent :: String -> Int -> String
lineContent content linenum = lines content !! linenum

-- Print line of source where error occured and show error message in context
-- Note: this is also called on a parser error.
fancyErrorMessage :: Posn -> String -> String -> String
-- (-1, -1) means no specific location
fancyErrorMessage (-1, -1) _ msg = "At end of input: error: " ++ msg
fancyErrorMessage (row, col) source msg
    = "Line " ++ (show row) ++ " column " ++ (show col) ++ ": error: " ++ msg ++
        "\n\n" ++ (lineContent source (row - 1)) ++
        "\n" ++ (take (col - 1) (repeat ' ')) ++ "^ error here"

-- Wrapper to allow fancyErrorMessage to be called directly from Alex, while
-- below is called by semantic analysis.
semanticError :: SemanticError -> String -> String
semanticError err source
  = fancyErrorMessage (errPosn err) source (show err)
