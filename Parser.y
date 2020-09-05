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
-- S/R warnings due to some of the optional components; shift action is fine
%expect 5

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

%left or
%left and
%left not
%nonassoc '=' '!=' '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/'
%left negate

%%
{- Regarding lists:
As per the happy manual, we write our list productions with left recursion to
better handle long sequences. This means the production is in reverse order
though, so we apply reverse whenever list productions are used. That trick is
stolen from GHC's happy grammar.
-}

Program     : RecordDecs ArrayDecs ProcDecs                 { Program (reverse $1) (reverse $2) (reverse $3) }

RecordDecs  : {- empty -}                                   { [] }
            | RecordDec                                     { [$1] }
            | RecordDecs RecordDec                          { $2 : $1 }

RecordDec   : record '{' FieldDecs '}' id ';'               { RecordDec $5 (reverse $3) }

FieldDecs   : FieldDec                                      { [$1] }
            | FieldDecs ';' FieldDec                        { $3 : $1 }

FieldDec    : boolean id                                    { FieldDec $2 BoolType }
            | integer id                                    { FieldDec $2 IntType }

ArrayDecs   : {- empty -}                                   { [] }
            | ArrayDec                                      { [$1] }
            | ArrayDecs ArrayDec                            { $2 : $1 }

ArrayDec    : array '[' integer_lit ']' ArrayType id ';'    { ArrayDec $6 $5 $3 }

ArrayType   : boolean                                       { BoolType }
            | integer                                       { IntType }
            | id                                            { (AliasType $1) }

ProcDecs    : ProcDec                                       { [$1] }
            | ProcDecs ProcDec                              { $2 : $1 }

ProcDec     : procedure id '(' Parameters ')' LocalVarDecs '{' Statements '}'   { Proc $2 (reverse $4) (reverse $6) (reverse $8) }

Parameters  : {- empty -}                                   { [] }
            | Parameter                                     { [$1] }
            | Parameters ',' Parameter                      { $3 : $1 }

Parameter   : id id                                         { RefParam $2 (AliasType $1) }
            | boolean id                                    { RefParam $2 BoolType }
            | integer id                                    { RefParam $2 IntType }
            | boolean val id                                { ValParam $3 BoolType }
            | integer val id                                { ValParam $3 IntType }

LocalVarDecs : {- empty -}                                  { [] }
             | LocalVarDec                                  { [$1] }
             | LocalVarDecs LocalVarDec                     { $2 : $1 }

LocalVarDec : id LocalVars ';'                              { VarDec (reverse $2) (AliasType $1) }
            | boolean LocalVars ';'                         { VarDec (reverse $2) BoolType }
            | integer LocalVars ';'                         { VarDec (reverse $2) IntType }

LocalVars   : id                                            { [$1] }
            | LocalVars ',' id                              { $3 : $1 }

Statements  : Statement                                     { [$1] }
            | Statements Statement                          { $2 : $1 }

Statement   : Lvalue assign Expr ';'                        { Assign $1 $3 }
            | read Lvalue ';'                               { Read $2 }
            | write Expr ';'                                { Write $2 }
            | writeln Expr ';'                              { WriteLn $2 }
            | call id '(' Args ')' ';'                      { Call $2 (reverse $4) }
            | if Expr then Statements ElseClause fi         { If $2 (reverse $4) $5 }
            | while Expr do Statements od                   { While $2 (reverse $4) }

ElseClause  : {- empty -}                                   { [] }
            | else Statements                               { (reverse $2) }

Args        : {- empty -}                                   { [] }
            | Expr                                          { [$1] }
            | Args ',' Expr                                 { $3 : $1 }

Lvalue      : id                                            { LId $1 }
            | id '.' id                                     { LField $1 $3 }
            | id '[' Expr ']'                               { LArray $1 $3 }
            | id '[' Expr ']' '.' id                        { LArrayField $1 $3 $6 }

Expr        : Lvalue                                        { Lval $1 }
            | boolean_lit                                   { BoolLit $1 }
            | integer_lit                                   { IntLit $1 }
            | string_lit                                    { StrLit $1 }
            | '(' Expr ')'                                  { $2 }
            | Expr or Expr                                  { BinOpExpr Op_or $1 $3 }
            | Expr and Expr                                 { BinOpExpr Op_and $1 $3 }
            | not Expr                                      { PreOpExpr Op_not $2 }
            | Expr '=' Expr                                 { BinOpExpr Op_eq $1 $3 }
            | Expr '!=' Expr                                { BinOpExpr Op_neq $1 $3 }
            | Expr '<' Expr                                 { BinOpExpr Op_lt $1 $3 }
            | Expr '<=' Expr                                { BinOpExpr Op_lteq $1 $3 }
            | Expr '>' Expr                                 { BinOpExpr Op_gt $1 $3 }
            | Expr '>=' Expr                                { BinOpExpr Op_gteq $1 $3 }
            | Expr '+' Expr                                 { BinOpExpr Op_plus $1 $3 }
            | Expr '-' Expr                                 { BinOpExpr Op_minus $1 $3 }
            | Expr '*' Expr                                 { BinOpExpr Op_mult $1 $3 }
            | Expr '/' Expr                                 { BinOpExpr Op_divide $1 $3 }
            | '-' Expr %prec negate                         { PreOpExpr Op_negate $2 }

{
parseError :: ([(AlexPosn, Token)], [String]) -> a
parseError ((AlexPn _ row col, t):ts, explist)
    = errorWithoutStackTrace ("Parse error on line " ++
            (show row) ++ ", column " ++ (show col) ++
            ". Got " ++ (show t) ++ " but expected one of: " ++
            (intercalate ", " explist))
}
