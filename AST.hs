module AST where

import Data.List (intercalate)

-----------------------------------
-- Specification of an AST for Roo
-----------------------------------

type Ident = String

data Program
    = Program [RecordDec] [ArrayDec] [Proc]
    deriving (Show, Eq)

data RecordDec
    = RecordDec Ident [FieldDec]
    deriving (Show, Eq)
    
data FieldDec
    = FieldDec Ident TypeName
    deriving (Show, Eq)

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

data VarDec
    = VarDec [Ident] TypeName
    deriving (Eq)

instance Show VarDec where
    show (VarDec idents typename) = (show typename) ++ " " ++ (intercalate ", " idents)

data Stmt 
    = Assign LValue Expr
    | Read LValue
    | Write Expr
    | WriteLn Expr
    | Call Ident [Expr]
    | If Expr [Stmt] [Stmt]
    | While Expr [Stmt]
    deriving (Show, Eq)

data TypeName
    = BoolType
    | IntType 
    | AliasType String
    deriving (Eq)

instance Show TypeName where
    show BoolType = "boolean"
    show IntType = "integer"
    show (AliasType name) = name

data LValue 
    = LId Ident
    | LField Ident Ident
    | LArray Ident Expr
    | LArrayField Ident Expr Ident
    deriving (Show, Eq)

data Expr
    = Lval LValue
    | BoolLit Bool
    | IntLit Integer
    | StrLit String
    | BinOpExpr BinOp Expr Expr
    | Lnot Expr
    | Negate Expr
    deriving (Show, Eq)

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
    deriving (Show, Eq)
