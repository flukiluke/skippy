-- Skippy, a compiler for the Roo language.
--
-- Submitted for assignment 3 of COMP90045, 2020
-- By Luke Ceddia [lceddia] and Ben Harper [bharper1]
-- 28 October 2020
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
type Posn = (Int, Int)

data Program
    = Program [RecordDec] [ArrayDec] [Proc]
    deriving (Show, Eq)

data RecordDec
    = RecordDec Posn Ident [FieldDec]
    deriving (Show, Eq)
    
data FieldDec
    = FieldDec Posn Ident TypeName
    deriving (Eq)

instance Show FieldDec where
    show (FieldDec _ ident typename) = (show typename) ++ " " ++ ident

data ArrayDec
    = ArrayDec Posn Ident TypeName Int
    deriving (Show, Eq)

data Proc
    = Proc Posn Ident [Parameter] [VarDec] [Stmt]
    deriving (Show, Eq)

data Parameter
    = RefParam Posn Ident TypeName
    | ValParam Posn Ident TypeName
    deriving (Eq)

instance Show Parameter where
    show (RefParam _ ident typename) = (show typename) ++ " " ++ ident
    show (ValParam _ ident typename) = (show typename) ++ " val " ++ ident

data VarDec
    = VarDec {
        varPosn :: Posn,
        varDecNames :: [Ident],
        varDecType :: TypeName }
    deriving (Eq)

instance Show VarDec where
    show (VarDec _ idents typename)
      = (show typename) ++ " " ++ (intercalate ", " idents)

data Stmt 
    = Assign Posn LValue Expr            -- Assign Expr to LValue
    | Read Posn LValue                   -- Read number input LValue
    | Write Posn Expr                    -- Write expr to output
    | WriteLn Posn Expr                  -- Write expr to output, with new line
    | Call Posn Ident [Expr]             -- Call procedure Ident with parameters [Expr]
    | If Posn Expr [Stmt] [Stmt]         -- Execute first [Stmt] if Expr is true,
                                    -- second [Expr] otherwise
    | While Posn Expr [Stmt]             -- Execute [Stmt] while Expr is true
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
    = LId Posn Ident                      -- Basic variable
    | LField Posn Ident Ident             -- Record field
    | LArray Posn Ident Expr              -- Array element
    | LArrayField Posn Ident Expr Ident   -- Field of array element
    deriving (Show, Eq)

data Expr
    = Lval { exprPosn :: Posn, exprLValue :: LValue }
    | BoolLit { exprPosn :: Posn, exprBool :: Bool }
    | IntLit { exprPosn :: Posn, exprInt :: Int }
    | StrLit { exprPosn :: Posn, exprStr :: String }
    | BinOpExpr { exprPosn :: Posn,
                exprBinOp :: BinOp,
                exprLeft :: Expr,
                exprRight :: Expr }
    | PreOpExpr { exprPosn :: Posn,
                exprPreOp :: PreOp,
                exprRight :: Expr }
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
