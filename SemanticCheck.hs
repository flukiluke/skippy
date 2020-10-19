module SemanticCheck where

import Control.Monad
import qualified Data.Map.Strict as Map
import qualified AST
import SymbolTable
import SemanticErrors

type Result a = Either SemanticError a

checkProgram :: SymbolTable -> AST.Program -> Either String ()
checkProgram symtab program@(AST.Program _ _ procs) = case do
    mapM_ (checkProcedure symtab) procs
        of
            Left e -> Left (show e)
            Right _ -> Right ()

checkProcedure :: SymbolTable -> AST.Proc -> Result ()
checkProcedure symtab (AST.Proc name _ _ stmts) = do
    mapM_ (checkStmt symtab locals) stmts
        where
            locals = procSymTab $ getProc symtab name

checkStmt :: SymbolTable -> Locals -> AST.Stmt -> Result ()
checkStmt symtab locals (AST.Assign posn lvalue expr) = do
    leftType <- lvalType symtab locals lvalue
    rightType <- exprType symtab locals expr
    unless (leftType == rightType) $
        Left (TypeMismatch posn (show leftType) (show rightType))

checkStmt symtab locals (AST.Read lvalue) = lvalType symtab locals lvalue >> return ()

checkStmt symtab locals (AST.Write expr)
  = exprType symtab locals expr >> return ()

checkStmt symtab locals (AST.WriteLn expr)
  = exprType symtab locals expr >> return ()

checkStmt symtab locals (AST.Call name args) = do
    proc <- lookup' name (procedures symtab) $ UndeclaredSymbol name 0 0
    zipWithM_ (checkArg symtab locals) (procSig proc) args

checkStmt symtab locals (AST.If expr trues falses) = do
    condType <- exprType symtab locals expr
    unless (condType == BoolType) $
        Left (TypeMismatch "boolean" (show condType) 0 0)
    mapM (checkStmt symtab locals) trues
    mapM_ (checkStmt symtab locals) falses

checkStmt symtab locals (AST.While expr stmts) = do
    condType <- exprType symtab locals expr
    unless (condType == BoolType) $
        Left (TypeMismatch "boolean" (show condType) 0 0)
    mapM_ (checkStmt symtab locals) stmts

checkArg :: SymbolTable -> Locals -> Variable -> AST.Expr -> Result ()
checkArg symtab locals expected passed = do
    when (varByRef expected && not (isLval passed)) $
        Left (NotReference 0 0)
    let expectedType = varType expected
    passedType <- exprType symtab locals passed
    unless (expectedType == passedType) $
        Left (TypeMismatch (show expectedType) (show passedType) 0 0)

exprType :: SymbolTable -> Locals -> AST.Expr -> Result RooType
exprType symtab locals (AST.Lval lval) = lvalType symtab locals lval
exprType _ locals (AST.BoolLit _ _) = Right BoolType
exprType _ locals (AST.IntLit _) = Right IntType
exprType _ locals (AST.StrLit _) = Right StringType
exprType symtab locals (AST.BinOpExpr AST.Op_or l r)
  = logicalBinOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr AST.Op_and l r)
  = logicalBinOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr AST.Op_eq l r)
  = comparisonOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr AST.Op_neq l r)
  = comparisonOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr AST.Op_lt l r)
  = comparisonOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr AST.Op_lteq l r)
  = comparisonOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr AST.Op_gt l r)
  = comparisonOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr AST.Op_gteq l r)
  = comparisonOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr AST.Op_plus l r)
  = mathBinOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr AST.Op_minus l r)
  = mathBinOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr AST.Op_mult l r)
  = mathBinOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr AST.Op_divide l r)
  = mathBinOpType symtab locals l r
exprType symtab locals (AST.PreOpExpr AST.Op_not r)
  = prefixOpType BoolType symtab locals r
exprType symtab locals (AST.PreOpExpr AST.Op_negate r)
  = prefixOpType IntType symtab locals r

isLval :: AST.Expr -> Bool
isLval (AST.Lval _) = True
isLval _ = False

isRecordType :: RooType -> Bool
isRecordType (RecordType _) = True
isRecordType _ = False

lookup' :: Ord k => k -> Map.Map k a -> SemanticError -> Either SemanticError a
lookup' key map err
  = case Map.lookup key map of
      Just v -> Right v
      Nothing -> Left $ err

lvalType :: SymbolTable -> Locals -> AST.LValue -> Result RooType
lvalType symtab locals (AST.LId name) = do
    lval <- lookup' name locals $ UndeclaredSymbol name 0 0
    return $ varType lval

lvalType symtab locals (AST.LField recName fieldName) = do
    record <- lookup' recName locals $ UndeclaredSymbol recName 0 0
    let r@(RecordType fields) = varType record
    unless (isRecordType r) $
        Left (UnexpectedField 0 0)
    fieldDec <- lookup' fieldName fields $ UndeclaredSymbol fieldName 0 0
    let (_, fieldType) = fieldDec
    return fieldType

lvalType symtab locals (AST.LArray arrayName index) = do
    array <- lookup' arrayName locals $ UndeclaredSymbol arrayName 0 0
    let (ArrayType arrayType _) = varType array
    return arrayType

lvalType symtab locals (AST.LArrayField arrayName index fieldName) = do
    array <- lookup' arrayName locals $ UndeclaredSymbol arrayName 0 0
    let (ArrayType arrayType _) = varType array
    unless (isRecordType arrayType) $
        Left (UnexpectedField 0 0)
    return IntType

logicalBinOpType :: SymbolTable -> Locals -> AST.Expr -> AST.Expr -> Result RooType
logicalBinOpType = uniformBinOpType BoolType

mathBinOpType :: SymbolTable -> Locals -> AST.Expr -> AST.Expr -> Result RooType
mathBinOpType = uniformBinOpType IntType

uniformBinOpType :: RooType -> SymbolTable -> Locals -> AST.Expr -> AST.Expr -> Result RooType
uniformBinOpType expectedType symtab locals l r = do
    leftType <- exprType symtab locals l
    rightType <- exprType symtab locals r
    unless (leftType == expectedType) $
        Left (TypeMismatch (show expectedType) (show leftType) 0 0)
    unless (rightType == BoolType) $
        Left (TypeMismatch (show expectedType) (show rightType) 0 0)
    return expectedType

prefixOpType :: RooType -> SymbolTable -> Locals -> AST.Expr -> Result RooType
prefixOpType expectedType symtab locals r = do
    rightType <- exprType symtab locals r
    unless (rightType == expectedType) $
        Left (TypeMismatch (show expectedType) (show rightType) 0 0)
    return expectedType

comparisonOpType :: SymbolTable -> Locals -> AST.Expr -> AST.Expr -> Result RooType
comparisonOpType symtab locals l r = do
    leftType <- exprType symtab locals l
    rightType <- exprType symtab locals r
    unless (leftType == rightType) $
        Left (TypeMismatch (show leftType) (show rightType) 0 0)
    unless (leftType == BoolType || leftType == IntType) $
        Left (TypeMismatch "boolean or integer" (show leftType) 0 0)
    return BoolType

