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
    -- Types
    boolean         { IDENT "boolean" }
    integer         { IDENT "integer" }

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
