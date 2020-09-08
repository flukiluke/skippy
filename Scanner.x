-- Skippy, a compiler for the Roo language.
--
-- Submitted for assignment 1a of COMP90045, 2020
-- By Luke Ceddia [lceddia] and Ben Harper [bharper1]
-- 16 September 2020
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.
--
-- This is an Alex lexer for the Roo language.
{
module Scanner where

import Data.Text (pack, unpack, replace)
import Control.Monad (liftM)
}

%wrapper "monadUserState"

$digit      = 0-9
$symbol     = [\=\<\>\{\}\[\]\(\)\+\-\*\/\;\.\,]
@alpha      = [a-zA-Z]
@digits     = $digit+
@string     = \" ([^\"] | \\\")* \"
@ident      = @alpha (@alpha | $digit | \_ | \')*
@comment    = \# [^\n]* \n
@symops     = "<-" | "<=" | ">=" | "!="
@keywords   =
        and|array|boolean|call|do|else|fi|if|integer|not|
        od|or|procedure|read|record|then|val|while|write|writeln

rules :-
  $white+   ;    -- skip white space
  @comment  ;    -- skip comments
  @digits   { lex_int }
  @string   { lex_str }
  true      { lex_bool True }
  false     { lex_bool False }
  @symops   { lex_lit Symbol }
  $symbol   { lex_lit Symbol }
  @keywords { lex_lit Keyword }
  @ident    { lex_lit Identifier }

{
{-
Defined by Alex, here for reference:
type AlexInput = (AlexPosn, -- current position,
                  Char, -- previous char
                  [Byte], -- rest of the bytes for the current char
                  String) -- current input string
-}

alexEOF :: Alex (AlexPosn, Token)
alexEOF = return (undefined, EOF)

data Token
    = Identifier String
    | Keyword String
    | BooleanLit Bool
    | IntegerLit Integer
    | StringLit String
    | Symbol String
    | EOF
    deriving (Eq, Show)

-- Make token that whose parameter is just a literal string
lex_lit :: (String -> Token) -> AlexInput -> Int -> Alex (AlexPosn, Token)
lex_lit t (p,_,_,str) len = return (p, t . take len $ str)

-- Make IntegerLit token
lex_int :: AlexInput -> Int -> Alex (AlexPosn, Token)
lex_int (p,_,_,str) len = return (p, IntegerLit . read . take len $ str)

-- Make BooleanLit token
lex_bool :: Bool -> AlexInput -> Int -> Alex (AlexPosn, Token)
lex_bool b (p,_,_,_) _ = return (p, BooleanLit b)

-- Make quoted string literal
lex_str :: AlexInput -> Int -> Alex (AlexPosn, Token)
lex_str (p,_,_,str) len = return (p, StringLit . unpack
        -- replace escaped quote (\") with literal quote (")
        . (replace (pack "\\\"") (pack "\""))
        -- etc
        . (replace (pack "\\n") (pack "\n"))
        . (replace (pack "\\t") (pack "\t"))
        -- strip leading and trailing quotes
        . pack . tail . take (len - 1) $ str)

-- Used to hold the original input for error reporting
data AlexUserState = AlexUserState { original :: String }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState ""

setOriginalInput :: String -> Alex ()
setOriginalInput = alexSetUserState . AlexUserState

lineContent :: String -> Int -> String
lineContent content linenum = lines content !! linenum

fancyErrorMessage :: AlexPosn -> String -> String -> String
fancyErrorMessage (AlexPn _ row col) source msg
    = "Line " ++ (show row) ++ " column " ++ (show col) ++ ": error: " ++ msg ++
        "\n\n" ++ (lineContent source (row - 1)) ++
        "\n" ++ (take (col - 1) (repeat ' ')) ++ "^ error here"
        
-- Alex insists on hardcoding a call from alexMonadScan to alexError with no
-- way to intercept it, so we give our own alexMonadScan that calls our own
-- error function (this function basically copied from the standard alex
-- generated code).
alexMonadScan' :: Alex (AlexPosn, Token)
alexMonadScan' = do
    inp <- alexGetInput
    sc <- alexGetStartCode
    case alexScan inp sc of
        AlexEOF -> alexEOF
        AlexError (AlexPn offset row col,c,_,_)
            -> alexError' (AlexPn offset row (col - 1))
                          ("Unexpected '" ++ c : "' here")
        AlexSkip inp' len -> do
            alexSetInput inp'
            alexMonadScan'
        AlexToken inp' len action -> do
            alexSetInput inp'
            action (ignorePendingBytes inp) len

scan :: String -> Alex a -> Either String a
scan input a = runAlex input (setOriginalInput input >> a)

alexError' :: AlexPosn -> String -> Alex a
alexError' p msg = Alex (\s -> let source = original . alex_ust $ s
                               in Left $ fancyErrorMessage p source msg)
}
