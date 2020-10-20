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
checkProcedure symtab (AST.Proc _ name _ _ stmts) = do
    mapM_ (checkStmt symtab locals) stmts
        where
            locals = procSymTab $ getProc symtab name

checkStmt :: SymbolTable -> Locals -> AST.Stmt -> Result ()
checkStmt symtab locals (AST.Assign posn lvalue expr) = do
    leftType <- lvalType symtab locals lvalue
    rightType <- exprType symtab locals expr
    unless (leftType == rightType) $
        Left (TypeMismatch posn (show leftType) (show rightType))

checkStmt symtab locals (AST.Read _ lvalue)
  = lvalType symtab locals lvalue >> return ()

checkStmt symtab locals (AST.Write _ expr)
  = exprType symtab locals expr >> return ()

checkStmt symtab locals (AST.WriteLn _ expr)
  = exprType symtab locals expr >> return ()

checkStmt symtab locals (AST.Call posn name args) = do
    proc <- lookup' name (procedures symtab) $ UndeclaredSymbol posn name
    zipWithM_ (checkArg symtab locals) (procSig proc) args

checkStmt symtab locals (AST.If posn expr trues falses) = do
    condType <- exprType symtab locals expr
    unless (condType == BoolType) $
        Left (TypeMismatch posn "boolean" (show condType))
    mapM (checkStmt symtab locals) trues
    mapM_ (checkStmt symtab locals) falses

checkStmt symtab locals (AST.While posn expr stmts) = do
    condType <- exprType symtab locals expr
    unless (condType == BoolType) $
        Left (TypeMismatch posn "boolean" (show condType))
    mapM_ (checkStmt symtab locals) stmts

checkArg :: SymbolTable -> Locals -> Variable -> AST.Expr -> Result ()
checkArg symtab locals expected passed = do
    when (varByRef expected && not (isLval passed)) $
        Left (NotReference $ AST.exprPosn passed)
    let expectedType = varType expected
    passedType <- exprType symtab locals passed
    unless (expectedType == passedType) $
        Left (TypeMismatch (AST.exprPosn passed) (show expectedType) (show passedType))

exprType :: SymbolTable -> Locals -> AST.Expr -> Result RooType
exprType symtab locals (AST.Lval _ lval) = lvalType symtab locals lval
exprType _ locals (AST.BoolLit _ _) = Right BoolType
exprType _ locals (AST.IntLit _ _) = Right IntType
exprType _ locals (AST.StrLit _ _) = Right StringType
exprType symtab locals (AST.BinOpExpr _ AST.Op_or l r)
  = logicalBinOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr _ AST.Op_and l r)
  = logicalBinOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr _ AST.Op_eq l r)
  = comparisonOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr _ AST.Op_neq l r)
  = comparisonOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr _ AST.Op_lt l r)
  = comparisonOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr _ AST.Op_lteq l r)
  = comparisonOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr _ AST.Op_gt l r)
  = comparisonOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr _ AST.Op_gteq l r)
  = comparisonOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr _ AST.Op_plus l r)
  = mathBinOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr _ AST.Op_minus l r)
  = mathBinOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr _ AST.Op_mult l r)
  = mathBinOpType symtab locals l r
exprType symtab locals (AST.BinOpExpr _ AST.Op_divide l r)
  = mathBinOpType symtab locals l r
exprType symtab locals (AST.PreOpExpr _ AST.Op_not r)
  = prefixOpType BoolType symtab locals r
exprType symtab locals (AST.PreOpExpr _ AST.Op_negate r)
  = prefixOpType IntType symtab locals r

isLval :: AST.Expr -> Bool
isLval (AST.Lval _ _) = True
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
lvalType symtab locals (AST.LId posn name) = do
    lval <- lookup' name locals $ UndeclaredSymbol posn name
    return $ varType lval

lvalType symtab locals (AST.LField posn recName fieldName) = do
    record <- lookup' recName locals $ UndeclaredSymbol posn recName
    let r@(RecordType fields) = varType record
    unless (isRecordType r) $
        Left (UnexpectedField posn)
    fieldDec <- lookup' fieldName fields $ UndeclaredSymbol posn fieldName
    let (_, fieldType) = fieldDec
    return fieldType

lvalType symtab locals (AST.LArray posn arrayName index) = do
    array <- lookup' arrayName locals $ UndeclaredSymbol posn arrayName
    let (ArrayType arrayType _) = varType array
    return arrayType

lvalType symtab locals (AST.LArrayField posn arrayName index fieldName) = do
    array <- lookup' arrayName locals $ UndeclaredSymbol posn arrayName
    let (ArrayType arrayType _) = varType array
    unless (isRecordType arrayType) $
        Left (UnexpectedField posn)
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
        Left (TypeMismatch (AST.exprPosn l) (show expectedType) (show leftType))
    unless (rightType == BoolType) $
        Left (TypeMismatch (AST.exprPosn r) (show expectedType) (show rightType))
    return expectedType

prefixOpType :: RooType -> SymbolTable -> Locals -> AST.Expr -> Result RooType
prefixOpType expectedType symtab locals r = do
    rightType <- exprType symtab locals r
    unless (rightType == expectedType) $
        Left (TypeMismatch (AST.exprPosn r) (show expectedType) (show rightType))
    return expectedType

comparisonOpType :: SymbolTable -> Locals -> AST.Expr -> AST.Expr -> Result RooType
comparisonOpType symtab locals l r = do
    leftType <- exprType symtab locals l
    rightType <- exprType symtab locals r
    unless (leftType == rightType) $
        Left (TypeMismatch (AST.exprPosn l) (show leftType) (show rightType))
    unless (leftType == BoolType || leftType == IntType) $
        Left (TypeMismatch (AST.exprPosn r) "boolean or integer" (show leftType))
    return BoolType

