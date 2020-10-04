-- Skippy, a compiler for the Roo language
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.

module CodeGen where

import AST
import SymbolTable
import qualified Data.Map.Lazy as Map
import Data.List (intercalate)

generateMachineCode :: Program -> IO()
generateMachineCode prog@(Program _ _ [(Proc _ _ _ stmts)]) = do
    -- one procedure should be enough for anyone
    putStrLn "call proc_main"
    putStrLn "halt"
    symbolTable <- return $ getSymbolTable prog
    (ProcSymbol table stack_size) <- return $ symbolTable Map.! "main"
    putStrLn "proc_main:"
    putStrLn $ "push_stack_frame " ++ show stack_size
    -- initialise variables
    putStrLn "int_const r0, 0"
    sequence $ map (\x -> putStrLn $ "store " ++ show x ++ ", r0") 
        $ take stack_size [0..]
    labels <- return [0..]
    mapM_ (generateStmtCode table) stmts
    putStrLn $ "pop_stack_frame " ++ show stack_size
    putStrLn "return"

-- placeholder, idk if this will become part of the symbol table?
getExprType :: Expr -> TypeName
getExprType (BoolLit _) = BoolType
getExprType (IntLit _) = IntType
getExprType (StrLit _) = AliasType ""
getExprType _ = IntType

-- expression, table, registers available for use
generateExprCode :: Expr -> SymbolTable -> [Int] -> IO()
generateExprCode (Lval (LId ident)) table (val_r:_)= do
    putStrLn $ "load r" ++ show val_r ++ ", " ++ show slot
        where (VarSymbol _ slot) = table Map.! ident

generateExprCode (BoolLit b) _ (register:_) = do
    putStrLn $ "int_const r" ++ (show register) ++ ", " ++ bool_lit b
        where bool_lit True  = "1"
              bool_lit False = "0"
generateExprCode (IntLit int) _ (register:_) = do
    putStrLn $ "int_const r" ++ (show register) ++ ", " ++ show int
generateExprCode (StrLit str) _ (register:_) = do
    putStrLn $ "string_const r" ++ (show register) ++ ", " ++ show str

generateExprCode (BinOpExpr op expr1 expr2) table (result_r:left_r:right_r:rs) = do
    generateExprCode expr1 table (left_r:rs)
    generateExprCode expr2 table (right_r:rs)
    putStrLn $ getOpStr op ++ " r" ++ (intercalate ", r"
        $ map show [result_r, left_r, right_r])
        where getOpStr Op_or     = "or"
              getOpStr Op_and    = "and"
              getOpStr Op_eq     = "cmp_eq_int"
              getOpStr Op_neq    = "cmp_ne_int"
              getOpStr Op_lt     = "cmp_lt_int"
              getOpStr Op_lteq   = "cmp_le_int"
              getOpStr Op_gt     = "cmp_gt_int"
              getOpStr Op_gteq   = "cmp_ge_int"
              getOpStr Op_plus   = "add_int"
              getOpStr Op_minus  = "sub_int"
              getOpStr Op_mult   = "mul_int"
              getOpStr Op_divide = "div_int"

generateExprCode (PreOpExpr op expr) table (result_r:tmp:rs) = do
    generateExprCode expr table (tmp:rs)
    putStrLn $ getOpStr op ++ " r" ++ show result_r ++ ", r" ++ show tmp
        where getOpStr Op_negate = "neg_int"
              getOpStr Op_not    = "not"


initialRegisters :: [Int]
initialRegisters = take 1024 [0..]

generateStmtCode :: SymbolTable -> Stmt -> IO()

generateStmtCode table (Assign (LId ident) expr) = do
    generateExprCode expr table (val_r:registers)
    -- get address
    putStrLn $ "load_address r" ++ addr_r ++ ", " ++ show slot
    putStrLn $ "store_indirect r" ++ addr_r ++ ", r" ++ show val_r
        where val_r = head initialRegisters
              addr_r = show . head . tail $ initialRegisters
              registers = tail . tail $ initialRegisters
              (VarSymbol _ slot) = table Map.! ident
    

generateStmtCode table (Read lval@(LId ident)) = do
    putStrLn $ "call_builtin " ++ readBuiltin
    putStrLn $ "load_address r1, " ++ show slot
    putStrLn "store_indirect r1, r0"
        where (VarSymbol _ slot) = table Map.! ident
              readBuiltin =  case (getExprType (Lval lval)) of
                                BoolType -> "read_bool"
                                IntType  -> "read_int"
                                _ -> "error"


generateStmtCode table (Write expr) = do
    generateExprCode expr table initialRegisters
    putStrLn $ "call_builtin " ++ printBuiltin
        where printBuiltin =  case (getExprType expr) of
                                BoolType -> "print_bool"
                                IntType  -> "print_int"
                                _ -> "print_string"


generateStmtCode table (WriteLn expr) = do
    generateExprCode expr table initialRegisters
    putStrLn $ "call_builtin " ++ printBuiltin
    putStrLn $ "call_builtin print_newline"
        where printBuiltin =  case (getExprType expr) of
                                BoolType -> "print_bool"
                                IntType  -> "print_int"
                                _ -> "print_string"

generateStmtCode table (Call ident expr) = do
    return ()

generateStmtCode table (If expr stmts else_stmts label) = do
    generateExprCode expr table initialRegisters
    putStrLn $ "branch_on_false r0, label_" ++ label ++ "_0"
    mapM_ (generateStmtCode table) stmts
    putStrLn $ "branch_uncond label_" ++ label ++ "_1"
    putStrLn $ "label_" ++ label ++ "_0:"
    mapM_ (generateStmtCode table) else_stmts
    putStrLn $ "label_" ++ label ++ "_1:"


generateStmtCode table (While expr stmts label) = do
    putStrLn $ "label_" ++ label ++ "_0:"
    generateExprCode expr table initialRegisters
    putStrLn $ "branch_on_false r0, label_" ++ label ++ "_1"
    mapM_ (generateStmtCode table) stmts
    putStrLn $ "branch_uncond label_" ++ label ++ "_0"
    putStrLn $ "label_" ++ label ++ "_1:"

placeLabel :: IO()
placeLabel = do
    putStrLn $ "label_" ++ show 0
