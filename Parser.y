{
module Parser where
import Scanner
import AST
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    -- Constants
    bool_const      { BOOL_CONST $$ }
    int_const       { INT_CONST $$ }
    str_const       { STR_CONST $$ }
    -- Types & Declarations
    array           { IDENT "array" }
    boolean         { IDENT "boolean" }
    integer         { IDENT "integer" }
    procedure       { IDENT "procedure" }
    record          { IDENT "record" }
    val             { IDENT "val" }
    -- Atomic statements
    assign          { ASSIGN }
    read            { IDENT "read" }
    write           { IDENT "write" }
    writeln         { IDENT "writeln" }
    call            { IDENT "call" }
    -- Compound statements
    while           { IDENT "while" }
    do              { IDENT "do" }
    od              { IDENT "od" }
    if              { IDENT "if" }
    then            { IDENT "then" }
    else            { IDENT "else" }
    fi              { IDENT "fi" }
    -- Operators
    and             { IDENT "and" }
    or              { IDENT "or" }
    not             { IDENT "not" }
    '='             { CHARSYM '=' }
    '!='            { COMP_NEQ }
    '<'             { CHARSYM '<' }
    '<='            { COMP_LTEQ }
    '>'             { CHARSYM '>' }
    '>='            { COMP_GTEQ }
    '+'             { CHARSYM '+' }
    '-'             { CHARSYM '-' }
    '*'             { CHARSYM '*' }
    '/'             { CHARSYM '/' }
    -- Punctuation
    '('             { CHARSYM '(' }
    ')'             { CHARSYM ')' }
    '['             { CHARSYM '[' }
    ']'             { CHARSYM ']' }
    '.'             { CHARSYM '.' }
    ';'             { CHARSYM ';' }
    -- Everything else
    id              { IDENT $$ }

%%


Exp     : Type Value        { ($1, $2) }

Type    : boolean           { BoolType }
        | integer           { IntType  }

Value   : bool_const        { BoolConst $1 }
        | int_const         { IntConst $1 }
        | str_const         { StrConst $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
