{
module Scanner where
}

%wrapper "posn"

$digit      = 0-9
$symbol     = [\=\<\>\{\}\[\]\(\)\+\-\*\/\;\.\,]
@alpha      = [a-zA-Z]
@digits     = $digit+
-- This doesn't handle escaped backslashes I think
@string     = \" [^\"]* \"
@ident      = @alpha (@alpha | $digit | \_ | \')*
@comment    = \# [^\n]* \n
@symops     = "<-" | "<=" | ">=" | "!="
@keywords   =
        and|array|assign|boolean|call|do|else|fi|if|integer|not|
        od|or|procedure|read|record|then|val|while|write|writeln

rules :-
  $white+   ;    -- skip white space
  @comment  ;    -- skip comments
  @digits   { (\p s -> (p, IntegerLit (read s :: Int))) }
  @string   { (\p s -> (p, StringLit s)) }
  true      { (\p s -> (p, BooleanLit True)) }
  false     { (\p s -> (p, BooleanLit False)) }
  @symops   { (\p s -> (p, Symbol s)) }
  $symbol   { (\p s -> (p, Symbol s)) }
  @keywords { (\p s -> (p, Keyword s)) }
  @ident    { (\p s -> (p, Identifier s)) }

{
data Token
    = Identifier String
    | Keyword String
    | BooleanLit Bool
    | IntegerLit Int
    | StringLit String
    | Symbol String
    deriving (Eq, Show)
}

