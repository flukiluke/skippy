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
    '{'             { CHARSYM '{' }
    '}'             { CHARSYM '}' }
    '['             { CHARSYM '[' }
    ']'             { CHARSYM ']' }
    '.'             { CHARSYM '.' }
    ';'             { CHARSYM ';' }
    ','             { CHARSYM ',' }
    -- Everything else
    id              { IDENT $$ }

%%

Program     : RecordDefs ArrayDefs ProcDefs                 {}

RecordDefs  : {- empty -}                                   {}
            | RecordDef                                     {}
            | RecordDefs RecordDef                          {}

RecordDef   : record '{' FieldDecs '}' id ';'               {}

FieldDecs   : FieldDec                                      {}
            | FieldDecs ';' FieldDec                        {}

FieldDec    : boolean id                                    {}
            | integer id                                    {}

ArrayDefs   : {- empty -}                                   {}
            | ArrayDef                                      {}
            | ArrayDefs ArrayDef                            {}

ArrayDef    : array '[' int_const ']' ArrayType id ';'      {}

ArrayType   : boolean                                       {}
            | integer                                       {}
            | id                                            {}

ProcDefs    : ProcDef                                       {}
            | ProcDefs ProcDef                              {}

ProcDef     : procedure id '(' Parameters ')' LocalVarDecs '{' Statements '}'   {}

Parameters  : {- empty -}                                   {}
            | Parameter                                     {}
            | Parameters ',' Parameter                      {}

Parameter   : id id                                         {}
            | boolean id                                    {}
            | integer id                                    {}
            | boolean val id                                {}
            | integer val id                                {}

LocalVarDecs : {- empty -}                                  {}
             | LocalVarDec                                  {}
             | LocalVarDecs LocalVarDec                     {}

LocalVarDec : id LocalVars ';'                              {}
            | boolean LocalVars ';'                         {}
            | integer LocalVars ';'                         {}

LocalVars   : id                                            {}
            | LocalVars ',' id                              {}

Statements  : Statement                                     {}
            | Statements Statement                          {}

Statement   : Lvalue assign Expr ';'                        {}
            | read Lvalue ';'                               {}
            | write Lvalue ';'                              {}
            | writeln Lvalue ';'                            {}
            | call id '(' Args ')' ';'                      {}
            | if Expr then Statements ElseClause fi         {}
            | while Expr do Statements od                   {}

ElseClause  : {- empty -}                                   {}
            | else Statements                               {}

Args        : {- empty -}                                   {}
            | Expr                                          {}
            | Args ',' Expr                                 {}

Lvalue      : id                                            {}
            | id '.' id                                     {}
            | id '[' Expr ']'                               {}
            | id '[' Expr ']' '.' id                        {}

Expr        : Lvalue                                        {}
            | bool_const                                    {}
            | int_const                                     {}
            | str_const                                     {}
            | '(' Expr ')'                                  {}
            | Expr or Expr                                  {}
            | Expr and Expr                                 {}
            | not Expr                                      {}
            | Expr '=' Expr                                 {}
            | Expr '!=' Expr                                {}
            | Expr '<' Expr                                 {}
            | Expr '<=' Expr                                {}
            | Expr '>' Expr                                 {}
            | Expr '>=' Expr                                {}
            | Expr '+' Expr                                 {}
            | Expr '-' Expr                                 {}
            | Expr '*' Expr                                 {}
            | Expr '/' Expr                                 {}
            | '-' Expr                                      {}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
