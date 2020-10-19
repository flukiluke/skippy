module SemanticCheck where

import qualified AST
import qualified SymbolTable as SymTab
import SemanticErrors

type Result = Either SemanticError ()

checkProgram :: SymTab.SymbolTable -> AST.Program -> Either String ()
checkProgram symtab program@(AST.Program _ _ procs) = case do
    mapM_ (checkProcedure symtab) procs
        of
            Left e -> Left (show e)
            Right _ -> Right ()

checkProcedure :: SymTab.SymbolTable -> AST.Proc -> Result
checkProcedure symtab (AST.Proc name _ _ stmts) = do
    mapM_ (checkStmt symtab locals) stmts
        where
            locals = SymTab.procSymTab $ SymTab.getProc symtab name

checkStmt :: SymTab.SymbolTable -> SymTab.Locals -> AST.Stmt -> Result
checkStmt symtab locals (AST.Assign lvalue expr)
  = Left $ UnimplementedFeature 0 0

checkStmt symtab locals (AST.Read lvalue)
  = Left $ UnimplementedFeature 0 0

checkStmt symtab locals (AST.Write expr)
  = Left $ UnimplementedFeature 0 0

checkStmt symtab locals (AST.WriteLn expr)
  = Left $ UnimplementedFeature 0 0

checkStmt symtab locals (AST.Call name args)
  = Left $ UnimplementedFeature 0 0

checkStmt symtab locals (AST.If expr trues falses)
  = Left $ UnimplementedFeature 0 0

checkStmt symtab locals (AST.While expr stmts)
  = Left $ UnimplementedFeature 0 0
