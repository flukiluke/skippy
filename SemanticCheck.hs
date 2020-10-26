-- Skippy, a compiler for the Roo language
--
-- Submitted for assignment 3 of COMP90045, 2020
-- By Luke Ceddia [lceddia] and Ben Harper [bharper1]
-- 28 October 2020
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.
--
-- This module determines the semantic well-formedness of a program given the
-- AST and symbol table. Specifically it checks:
--  * Types of operator arguments are legal
--  * Types of assignments match
--  * Array indexes are integers
--  * If and While guards are booleans
--  * Existence of all referenced variables, procedures and record fields
--  * Procedure calls have the correct type and number of parameters
--  * Parameters passed as a non-val argument are actually lvalues
--  * Existence and signature of the main() procedure

module SemanticCheck where

import Control.Monad
import qualified Data.Map.Strict as Map
import qualified AST
import SymbolTable
import ErrorHandling

-- Evaluate in the Either monad so we can report an error with Left.
type Result a = Either SemanticError a

-- Main entry point to check a program.
checkProgram :: SymbolTable -> AST.Program -> Result ()
checkProgram symtab program@(AST.Program _ _ procs) = do
    mapM (checkProcedure symtab) procs
    checkMainProcedure symtab

-- Check for existence and signature of main()
checkMainProcedure :: SymbolTable -> Result ()
checkMainProcedure symtab = do
    -- (-1, -1) error location prevents error handler from trying to
    -- point to a specific place in the source.
    proc <- lookup' "main" (procedures symtab) $ NoMainProcedure (-1, -1)
    unless ((length . procSig $ proc) == 0) $ Left (NoMainProcedure (-1, -1))

-- Check all statements in a procedure
checkProcedure :: SymbolTable -> AST.Proc -> Result ()
checkProcedure symtab (AST.Proc _ name _ _ stmts) = do
    mapM_ (checkStmt symtab locals) stmts
        where
            locals = procSymTab $ getProc symtab name

-- Check a single statement
checkStmt :: SymbolTable -> Locals -> AST.Stmt -> Result ()

-- Types of assignment must match
checkStmt symtab locals (AST.Assign posn lvalue expr) = do
    leftType <- lvalType locals lvalue
    rightType <- exprType locals expr
    unless (leftType == rightType) $
        Left (TypeMismatch posn (show leftType) (show rightType))

-- Read argument being an lvalue is guaranteed by parser grammar
checkStmt symtab locals (AST.Read _ lvalue)
  = lvalType locals lvalue >> return ()

-- All Write arguments are valid
checkStmt symtab locals (AST.Write _ expr)
  = exprType locals expr >> return ()

-- As above
checkStmt symtab locals (AST.WriteLn _ expr)
  = exprType locals expr >> return ()

-- Check Call is to extant procedure and all arguments align
checkStmt symtab locals (AST.Call posn name args) = do
    proc <- lookup' name (procedures symtab) $ UndeclaredSymbol posn name
    zipWithM_ (checkArg symtab locals) (procSig proc) args

-- Check If has boolean guard then check branches
checkStmt symtab locals (AST.If posn expr trues falses) = do
    condType <- exprType locals expr
    unless (condType == BoolType) $
        Left (TypeMismatch posn "boolean" (show condType))
    mapM (checkStmt symtab locals) trues
    mapM_ (checkStmt symtab locals) falses

-- Check While has boolean guard then check body
checkStmt symtab locals (AST.While posn expr stmts) = do
    condType <- exprType locals expr
    unless (condType == BoolType) $
        Left (TypeMismatch posn "boolean" (show condType))
    mapM_ (checkStmt symtab locals) stmts

-- Ensure an argument has the same type as expected parameter and, if not
-- a val argument, is an lvalue.
checkArg :: SymbolTable -> Locals -> Variable -> AST.Expr -> Result ()
checkArg symtab locals expected passed = do
    when (varByRef expected && not (isLval passed)) $
        Left (NotReference $ AST.exprPosn passed)
    let expectedType = varType expected
    passedType <- exprType locals passed
    unless (expectedType == passedType) $
        Left (TypeMismatch
                (AST.exprPosn passed)
                (show expectedType)
                (show passedType))

-- Get type of expression. Operators fall into one of a few categories.
exprType :: Locals -> AST.Expr -> Result RooType
exprType locals (AST.Lval _ lval) = lvalType locals lval
exprType locals (AST.BoolLit _ _) = Right BoolType
exprType locals (AST.IntLit _ _) = Right IntType
exprType locals (AST.StrLit _ _) = Right StringType
exprType locals (AST.BinOpExpr _ AST.Op_or l r)
  = logicalBinOpType locals l r
exprType locals (AST.BinOpExpr _ AST.Op_and l r)
  = logicalBinOpType locals l r
exprType locals (AST.BinOpExpr _ AST.Op_eq l r)
  = comparisonOpType locals l r
exprType locals (AST.BinOpExpr _ AST.Op_neq l r)
  = comparisonOpType locals l r
exprType locals (AST.BinOpExpr _ AST.Op_lt l r)
  = comparisonOpType locals l r
exprType locals (AST.BinOpExpr _ AST.Op_lteq l r)
  = comparisonOpType locals l r
exprType locals (AST.BinOpExpr _ AST.Op_gt l r)
  = comparisonOpType locals l r
exprType locals (AST.BinOpExpr _ AST.Op_gteq l r)
  = comparisonOpType locals l r
exprType locals (AST.BinOpExpr _ AST.Op_plus l r)
  = mathBinOpType locals l r
exprType locals (AST.BinOpExpr _ AST.Op_minus l r)
  = mathBinOpType locals l r
exprType locals (AST.BinOpExpr _ AST.Op_mult l r)
  = mathBinOpType locals l r
exprType locals (AST.BinOpExpr _ AST.Op_divide l r)
  = mathBinOpType locals l r
exprType locals (AST.PreOpExpr _ AST.Op_not r)
  = prefixOpType BoolType locals r
exprType locals (AST.PreOpExpr _ AST.Op_negate r)
  = prefixOpType IntType locals r

-- Determine type of an Lvalue
lvalType :: Locals -> AST.LValue -> Result RooType
-- Plain variables are a simple lookup
lvalType locals (AST.LId posn name) = do
    lval <- lookup' name locals $ UndeclaredSymbol posn name
    return $ varType lval

-- Record fields also need to check the field exists
lvalType locals (AST.LField posn recName fieldName) = do
    record <- lookup' recName locals $ UndeclaredSymbol posn recName
    let r@(RecordType fields) = varType record
    unless (isRecordType r) $
        Left (UnexpectedField posn)
    fieldDec <- lookup' fieldName fields $ UndeclaredSymbol posn fieldName
    let (_, fieldType) = fieldDec
    return fieldType

-- Arrays need to check index is of integer type
lvalType locals (AST.LArray posn arrayName index) = do
    array <- lookup' arrayName locals $ UndeclaredSymbol posn arrayName
    let (ArrayType arrayType _) = varType array
    indexType <- exprType locals index 
    unless (indexType == IntType) $ Left (BadIndex posn)
    return arrayType

-- Combination of above
lvalType locals (AST.LArrayField posn arrayName index fieldName) = do
    array <- lookup' arrayName locals $ UndeclaredSymbol posn arrayName
    let (ArrayType arrayType _) = varType array
    unless (isRecordType arrayType) $
        Left (UnexpectedField posn)
    let (RecordType fields) = arrayType
    fieldDec <- lookup' fieldName fields $ UndeclaredSymbol posn fieldName
    let (_, fieldType) = fieldDec
    return fieldType

-- Binary operators that take two booleans and produce a boolean
logicalBinOpType :: Locals -> AST.Expr -> AST.Expr -> Result RooType
logicalBinOpType = uniformBinOpType BoolType

-- Binary operators that take two integer and produce an integer
mathBinOpType :: Locals -> AST.Expr -> AST.Expr -> Result RooType
mathBinOpType = uniformBinOpType IntType

-- Generalised version of above two functions
uniformBinOpType :: RooType -> Locals -> AST.Expr -> AST.Expr -> Result RooType
uniformBinOpType expectedType locals l r = do
    leftType <- exprType locals l
    rightType <- exprType locals r
    unless (leftType == expectedType) $
        Left (TypeMismatch (AST.exprPosn l)
                (show expectedType) (show leftType))
    unless (rightType == expectedType) $
        Left (TypeMismatch (AST.exprPosn r)
                (show expectedType) (show rightType))
    return expectedType

-- Prefix operator that produces the same type it accepts
prefixOpType :: RooType -> Locals -> AST.Expr -> Result RooType
prefixOpType expectedType locals r = do
    rightType <- exprType locals r
    unless (rightType == expectedType) $
        Left (TypeMismatch (AST.exprPosn r) (show expectedType) (show rightType))
    return expectedType

-- Binary operator that takes booleans or integer and produces a boolean,
-- but both arguments must be of the same type.
comparisonOpType :: Locals -> AST.Expr -> AST.Expr -> Result RooType
comparisonOpType locals l r = do
    leftType <- exprType locals l
    rightType <- exprType locals r
    unless (leftType == rightType) $
        Left (TypeMismatch (AST.exprPosn l) (show leftType) (show rightType))
    unless (leftType == BoolType || leftType == IntType) $
        Left (TypeMismatch (AST.exprPosn r) "boolean or integer" (show leftType))
    return BoolType


-- Convenient utility functions
-- Is an Expr an Lval?
isLval :: AST.Expr -> Bool
isLval (AST.Lval _ _) = True
isLval _ = False

-- Is a type a Record?
isRecordType :: RooType -> Bool
isRecordType (RecordType _) = True
isRecordType _ = False

-- Wrapper for map lookup that returns the given SemanticError in an Either
-- if the lookup fails
lookup' :: Ord k => k -> Map.Map k a -> SemanticError -> Either SemanticError a
lookup' key map err
  = case Map.lookup key map of
      Just v -> Right v
      Nothing -> Left $ err

