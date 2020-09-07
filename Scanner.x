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
}

%wrapper "posn"

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
  @digits   { (\p s -> (p, IntegerLit (read s :: Integer))) }
  @string   { (\p s -> (p, StringLit . unpack
                           -- replace escaped quote (\") with literal quote (")
                           . (replace (pack "\\\"") (pack "\""))
                           -- etc
                           . (replace (pack "\\n") (pack "\n"))
                           . (replace (pack "\\t") (pack "\t"))
                           -- strip leading and trailing quotes
                           . pack . tail . init $ s)) }
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
    | IntegerLit Integer
    | StringLit String
    | Symbol String
    deriving (Eq, Show)
}
