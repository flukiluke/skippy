-- Skippy, a compiler for the Roo language.
--
-- Submitted for assignment 1b of COMP90045, 2020
-- By Luke Ceddia [lceddia] and Ben Harper [bharper1]
-- 16 September 2020
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.
--
-- This is the main Happy parser for the language. It is based on the examples
-- presented in the Happy manual.
{
module Parser where
import Data.List (intercalate)
import Scanner
import AST
}

%name parse
%tokentype { (AlexPosn, Token) }
%monad { Alex }
%lexer { lexwrap } { (_, EOF) }
%error { parseError }
-- Requires Happy version 1.19.7 or greater
%errorhandlertype explist

%attributetype { Attributes a }
%attribute node { a }
%attribute posn { (Int, Int) }

-- Note: the first element of each tuple is Alex position info
%token
    -- Literals
    boolean_lit     { (_, BooleanLit _) }
    integer_lit     { (_, IntegerLit _) }
    string_lit      { (_, StringLit _) }
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
    id              { (_, Identifier _) }

-- In order of precedence
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

Program     : RecordDecs ArrayDecs ProcDecs
                { $$ = Program (reverse $1) (reverse $2) (reverse $3) }

RecordDecs  : {- empty -}                                   { $$ = [] }
            | RecordDecs RecordDec                          { $$ = $2 : $1 }

RecordDec   : record '{' FieldDecs '}' id ';'
                { $$ = RecordDec (getPosn $1) (unIdent $5) (reverse $3) }

FieldDecs   : FieldDec                                      { $$ = [$1] }
            | FieldDecs ';' FieldDec                        { $$ = $3 : $1 }

FieldDec    : boolean id
                { $$ = FieldDec (getPosn $1) (unIdent $2) BoolType }
            | integer id
                { $$ = FieldDec (getPosn $1) (unIdent $2) IntType }

ArrayDecs   : {- empty -}                                   { $$ = [] }
            | ArrayDecs ArrayDec                            { $$ = $2 : $1 }

ArrayDec    : array '[' integer_lit ']' ArrayType id ';'
                { $$ = ArrayDec (getPosn $1) (unIdent $6) $5 (unIntegerLit $3) }

ArrayType   : boolean                                       { $$ = BoolType }
            | integer                                       { $$ = IntType }
            | id                                            { $$ = AliasType $ unIdent $1 }

ProcDecs    : ProcDec                                       { $$ = [$1] }
            | ProcDecs ProcDec                              { $$ = $2 : $1 }

ProcDec     : procedure id '(' Parameters ')' LocalVarDecs '{' Statements '}'
                { $$ = Proc (getPosn $1) (unIdent $2) (reverse $4) (reverse $6) (reverse $8) }

Parameters  : {- empty -}                                   { $$ = [] }
            | ParamList                                     { $$ = $1 }

ParamList   : Parameter                                     { $$ = [$1] }
            | ParamList ',' Parameter                       { $$ = $3 : $1 }

Parameter   : id id
                { $$ = RefParam (getPosn $1) (unIdent $2) (AliasType $ unIdent $1) }
            | boolean id
                { $$ = RefParam (getPosn $1) (unIdent $2) BoolType }
            | integer id                          
                { $$ = RefParam (getPosn $1) (unIdent $2) IntType }
            | boolean val id                      
                { $$ = ValParam (getPosn $1) (unIdent $3) BoolType }
            | integer val id                      
                { $$ = ValParam (getPosn $1) (unIdent $3) IntType }

LocalVarDecs : {- empty -}                        { $$ = [] }
             | LocalVarDecs LocalVarDec           { $$ = $2 : $1 }

LocalVarDec : id LocalVars ';'
                { $$ = VarDec (getPosn $1) (reverse $2) (AliasType $ unIdent $1) }
            | boolean LocalVars ';'
                { $$ = VarDec (getPosn $1) (reverse $2) BoolType }
            | integer LocalVars ';'
                { $$ = VarDec (getPosn $1) (reverse $2) IntType }

LocalVars   : id                                { $$ = [unIdent $1] }
            | LocalVars ',' id                  { $$ = (unIdent $3) : $1 }

Statements  : Statement                         { $$ = [$1] }
            | Statements Statement              { $$ = $2 : $1 }

Statement   : Lvalue assign Expr ';'            
                { $$ = Assign (getPosn $2) $1 $3 }
            | read Lvalue ';'                   
                { $$ = Read (getPosn $1) $2 }
            | write Expr ';'                    
                { $$ = Write (getPosn $1) $2 }
            | writeln Expr ';'                  
                { $$ = WriteLn (getPosn $1) $2 }
            | call id '(' Args ')' ';'          
                { $$ = Call (getPosn $1) (unIdent $2) (reverse $4) }
            | if Expr then Statements ElseClause fi
                { $$ = If (getPosn $1) $2 (reverse $4) $5 }
            | while Expr do Statements od       
                { $$ = While (getPosn $1) $2 (reverse $4) }
                                                

ElseClause  : {- empty -}                       { $$ = [] }
            | else Statements                   { $$ = (reverse $2) }

Args        : {- empty -}                       { $$ = [] }
            | Expr                              { $$ = [$1] }
            | Args ',' Expr                     { $$ = $3 : $1 }

Lvalue      : id                                { $$ = LId $$.posn (unIdent $1)
                                                ; $$.posn = getPosn $1
                                                }
            | id '.' id                         { $$ = LField $$.posn (unIdent $1) (unIdent $3)
                                                ; $$.posn = getPosn $1
                                                }
            | id '[' Expr ']'                   { $$ = LArray $$.posn (unIdent $1) $3
                                                ; $$.posn = getPosn $1
                                                }
            | id '[' Expr ']' '.' id            { $$ = LArrayField $$.posn (unIdent $1) $3 (unIdent $6)
                                                ; $$.posn = getPosn $1
                                                }

Expr        : Lvalue                            { $$ = Lval $$.posn $1
                                                ; $$.posn = $1.posn
                                                }
            | boolean_lit                       { $$ = BoolLit $$.posn $ unBooleanLit $1
                                                ; $$.posn = getPosn $1
                                                }
            | integer_lit                       { $$ = IntLit $$.posn $ unIntegerLit $1
                                                ; $$.posn = getPosn $1
                                                }
            | string_lit                        { $$ = StrLit $$.posn $ unStringLit $1
                                                ; $$.posn = getPosn $1
                                                }
            | '(' Expr ')'                      { $$ = $2
                                                ; $$.posn = getPosn $1
                                                }
            | Expr or Expr                      { $$ = BinOpExpr $$.posn Op_or $1 $3
                                                ; $$.posn = getPosn $2
                                                }
            | Expr and Expr                     { $$ = BinOpExpr $$.posn Op_and $1 $3
                                                ; $$.posn = getPosn $2
                                                }
            | not Expr                          { $$ = PreOpExpr $$.posn Op_not $2
                                                ; $$.posn = getPosn $1
                                                }
            | Expr '=' Expr                     { $$ = BinOpExpr $$.posn Op_eq $1 $3
                                                ; $$.posn = getPosn $2
                                                }
            | Expr '!=' Expr                    { $$ = BinOpExpr $$.posn Op_neq $1 $3
                                                ; $$.posn = getPosn $2
                                                }
            | Expr '<' Expr                     { $$ = BinOpExpr $$.posn Op_lt $1 $3
                                                ; $$.posn = getPosn $2
                                                }
            | Expr '<=' Expr                    { $$ = BinOpExpr $$.posn Op_lteq $1 $3
                                                ; $$.posn = getPosn $2
                                                }
            | Expr '>' Expr                     { $$ = BinOpExpr $$.posn Op_gt $1 $3
                                                ; $$.posn = getPosn $2
                                                }
            | Expr '>=' Expr                    { $$ = BinOpExpr $$.posn Op_gteq $1 $3
                                                ; $$.posn = getPosn $2
                                                }
            | Expr '+' Expr                     { $$ = BinOpExpr $$.posn Op_plus $1 $3
                                                ; $$.posn = getPosn $2
                                                }
            | Expr '-' Expr                     { $$ = BinOpExpr $$.posn Op_minus $1 $3
                                                ; $$.posn = getPosn $2
                                                }
            | Expr '*' Expr                     { $$ = BinOpExpr $$.posn Op_mult $1 $3
                                                ; $$.posn = getPosn $2
                                                }
            | Expr '/' Expr                     { $$ = BinOpExpr $$.posn Op_divide $1 $3
                                                ; $$.posn = getPosn $2
                                                }
            | '-' Expr %prec negate             { $$ = PreOpExpr $$.posn Op_negate $2
                                                ; $$.posn = getPosn $1
                                                }

{
-- This function gets called on a parse error. It tries to generate a list of
-- tokens that we expected (this may or may not be useful to the programmer).
-- It redirects to the alex error function because we ultimately want to
-- handle them in the same way.
parseError :: ((AlexPosn, Token), [String]) -> Alex a
parseError ((p, t), explist)
    = alexError' p (case (length explist) of
        0 -> "Unexpected " ++ (show t) ++ " here."
        1 -> "Got " ++ (show t) ++ " here but expected " ++ (head explist)
        _ -> "Got " ++ (show t) ++ " but expected one of: "
                ++ (intercalate ", " explist))

-- As recommended by the Happy manual, this wrapper makes the types of
-- everything match up.
lexwrap :: ((AlexPosn, Token) -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

unStringLit (_, StringLit x) = x
unIntegerLit (_, IntegerLit x) = x
unBooleanLit (_, BooleanLit x) = x
unIdent :: (AlexPosn, Token) -> String
unIdent (_, Identifier x) = x

getPosn :: (AlexPosn, Token) -> (Int, Int)
getPosn (AlexPn _ row col, _) = (row, col)
}
