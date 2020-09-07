-- Skippy, a compiler for the Roo language.
--
-- Submitted for assignment 1a of COMP90045, 2020
-- By Luke Ceddia [lceddia] and Ben Harper [bharper1]
-- 16 September 2020
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.
--
-- This is the structure of the Abstract Syntax Tree used parsing the input
-- program. It also includes some basic show definitions to assist pretty
-- printing.

module AST where

import Data.List (intercalate)

type Ident = String

data Program
    = Program [RecordDec] [ArrayDec] [Proc]
    deriving (Show, Eq)

data RecordDec
    = RecordDec Ident [FieldDec]
    deriving (Show, Eq)
    
data FieldDec
    = FieldDec Ident TypeName
    deriving (Eq)

instance Show FieldDec where
    show (FieldDec ident typename) = (show typename) ++ " " ++ ident

data ArrayDec
    = ArrayDec Ident TypeName Integer
    deriving (Show, Eq)

data Proc
    = Proc Ident [Parameter] [VarDec] [Stmt]
    deriving (Show, Eq)

data Parameter
    = RefParam Ident TypeName
    | ValParam Ident TypeName
    deriving (Eq)

instance Show Parameter where
    show (RefParam ident typename) = (show typename) ++ " " ++ ident
    show (ValParam ident typename) = (show typename) ++ " val " ++ ident

data VarDec
    = VarDec [Ident] TypeName
    deriving (Eq)

instance Show VarDec where
    show (VarDec idents typename)
      = (show typename) ++ " " ++ (intercalate ", " idents)

data Stmt 
    = Assign LValue Expr     -- Assign Expr to LValue
    | Read LValue            -- Read number input LValue
    | Write Expr             -- Write expr to output
    | WriteLn Expr           -- Write expr to output, with new line
    | Call Ident [Expr]      -- Call procedure Ident with parameters [Expr]
    | If Expr [Stmt] [Stmt]  -- Execute first [Stmt] if Expr is true,
                             -- second [Expr] otherwise
    | While Expr [Stmt]      -- Execute [Stmt] while Expr is true
    deriving (Show, Eq)

data TypeName
    = BoolType
    | IntType 
    | AliasType String       -- This is for record and array types
    deriving (Eq)

instance Show TypeName where
    show BoolType = "boolean"
    show IntType = "integer"
    show (AliasType name) = name

data LValue 
    = LId Ident                      -- Basic variable
    | LField Ident Ident             -- Record field
    | LArray Ident Expr              -- Array element
    | LArrayField Ident Expr Ident   -- Field of array element
    deriving (Show, Eq)

data Expr
    = Lval LValue
    | BoolLit Bool
    | IntLit Integer
    | StrLit String
    | BinOpExpr BinOp Expr Expr
    | PreOpExpr PreOp Expr
    deriving (Show, Eq)

-- Prefix operators
data PreOp
    = Op_negate
    | Op_not
    deriving (Eq)

instance Show PreOp where
    show Op_negate = "-"
    show Op_not = "not "

-- Binary infix operators
data BinOp 
    = Op_or
    | Op_and
    | Op_eq
    | Op_neq
    | Op_lt
    | Op_lteq
    | Op_gt
    | Op_gteq
    | Op_plus
    | Op_minus
    | Op_mult
    | Op_divide
    deriving (Eq)

instance Show BinOp where
    show Op_or = "or"
    show Op_and = "and"
    show Op_eq = "="
    show Op_neq = "!="
    show Op_lt = "<"
    show Op_lteq = "<="
    show Op_gt = ">"
    show Op_gteq = ">="
    show Op_plus = "+"
    show Op_minus = "-"
    show Op_mult = "*"
    show Op_divide = "/"
