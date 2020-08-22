{
module Scanner where
}

%wrapper "basic"

$digit       = 0-9
@alpha       = [a-zA-Z]
@digits      = $digit+
-- This doesn't handle escaped backslashes I think
@string      = \" [^\"]* \"
@ident       = @alpha (@alpha | $digit | \_ | \')*
@comment     = \# [^\n]* \n
@charsym     = [\=\<\>\{\}\[\]\(\)\+\-\*\/\;\.]

rules :-
  $white+    ;    -- skip white space
  @comment   ;    -- skip comments
  @digits    { \s -> INT_CONST (read s :: Int) }
  @string    { \s -> STR_CONST s }
  true       { \s -> BOOL_CONST True }
  false      { \s -> BOOL_CONST False }
  \<\-       { \s -> ASSIGN }
  \!\=       { \s -> COMP_NEQ }
  \<\=       { \s -> COMP_LTEQ }
  \>\=       { \s -> COMP_GTEQ }
  @charsym   { \s -> CHARSYM (head s) }
  @ident     { \s -> IDENT s }

{
data Token
  = INT_CONST Int | BOOL_CONST Bool | STR_CONST String
  | IDENT String | ASSIGN | COMP_NEQ | COMP_LTEQ | COMP_GTEQ
  | CHARSYM Char
    deriving (Eq, Show)
}

