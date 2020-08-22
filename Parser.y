{
module Parser where
import Data.List (intercalate)
import Scanner
import AST
}

%name parse
%tokentype { (AlexPosn, Token) }
%error { parseError }
%errorhandlertype explist

%token
    -- Literals
    boolean_lit     { (_, BooleanLit $$) }
    integer_lit     { (_, IntegerLit $$) }
    string_lit      { (_, StringLit $$) }
    -- Types & Declarations
    array           { (_, Keyword "array") }
    boolean         { (_, Keyword "boolean") }
    integer         { (_, Keyword "integer") }
    procedure       { (_, Keyword "procedure") }
    record          { (_, Keyword "record") }
    val             { (_, Keyword "val") }
    -- Atomic statements
    assign          { (_, Symbol "<-") }
    read            { (_, Keyword "read") }
    write           { (_, Keyword "write") }
    writeln         { (_, Keyword "writeln") }
    call            { (_, Keyword "call") }
    -- Compound statements
    while           { (_, Keyword "while") }
    do              { (_, Keyword "do") }
    od              { (_, Keyword "od") }
    if              { (_, Keyword "if") }
    then            { (_, Keyword "then") }
    else            { (_, Keyword "else") }
    fi              { (_, Keyword "fi") }
    -- Operators
    and             { (_, Keyword "and") }
    or              { (_, Keyword "or") }
    not             { (_, Keyword "not") }
    '='             { (_, Symbol "=") }
    '!='            { (_, Symbol "!=") }
    '<'             { (_, Symbol "<") }
    '<='            { (_, Symbol "<=") }
    '>'             { (_, Symbol ">") }
    '>='            { (_, Symbol ">=") }
    '+'             { (_, Symbol "+") }
    '-'             { (_, Symbol "-") }
    '*'             { (_, Symbol "*") }
    '/'             { (_, Symbol "/") }
    -- Punctuation
    '('             { (_, Symbol "(") }
    ')'             { (_, Symbol ")") }
    '{'             { (_, Symbol "{") }
    '}'             { (_, Symbol "}") }
    '['             { (_, Symbol "[") }
    ']'             { (_, Symbol "]") }
    '.'             { (_, Symbol ".") }
    ';'             { (_, Symbol ";") }
    ','             { (_, Symbol ",") }
    -- Everything else
    id              { (_, Identifier $$) }

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

ArrayDef    : array '[' integer_lit ']' ArrayType id ';'    {}

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
            | write Expr ';'                                {}
            | writeln Expr ';'                              {}
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
            | boolean_lit                                   {}
            | integer_lit                                   {}
            | string_lit                                    {}
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
parseError :: ([(AlexPosn, Token)], [String]) -> a
parseError ((AlexPn _ row col, t):ts, explist)
    = error ("Parse error on line " ++
            (show row) ++ ", column " ++ (show col) ++
            ". Got " ++ (show t) ++ " but expected one of: " ++
            (intercalate ", " explist))
}
