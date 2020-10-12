-- Skippy, a compiler for the Roo language
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.

module CodeGen where

import AST
import SymbolTable
import qualified Data.Map.Lazy as Map
import Data.List (intercalate)

data OzInstruction
    = OzCall String
    | OzLabel String
    | OzPrint TypeName
    | OzHalt
    | OzPushStackFrame Int
    | OzPopStackFrame Int
    | OzStore Int Int
    | OzStoreIndirect Int Int
    | OzLoad Int Int
    | OzLoadIndirect Int Int
    | OzIntConst Int Integer
    | OzBoolConst Int Bool
    | OzStringConst Int String
    | OzReturn
    | OzBinOp BinOp Int Int Int
    | OzUnaryOp PreOp Int Int
    | OzCallBuiltin String
    | OzLoadAddress Int Int
    | OzBranchOnFalse Int String
    | OzBranchUncond String
    | OzSubOffset Int Int Int

instance Show OzInstruction where
    show (OzCall p) = "call " ++ p
    show (OzLabel l) = l ++ ":"
    show (OzPrint t) = "call_builtin " ++ printBuiltin
        where printBuiltin = case t of
                                 BoolType -> "print_bool"
                                 IntType  -> "print_int"
                                 _ -> "print_string"
    show OzHalt = "halt"
    show (OzPushStackFrame x) = "push_stack_frame " ++ show x
    show (OzPopStackFrame x) = "pop_stack_frame " ++ show x
    show (OzStore target from) = "store " ++ show target ++ ", r" ++ show from
    show (OzStoreIndirect target from) = "store_indirect r" ++ show target
        ++ ", r" ++ show from
    show (OzLoad target from) = "load r" ++ show target ++ ", " ++ show from
    show (OzLoadIndirect target from) = "load_indirect r" ++ show target
        ++ ", r" ++ show from
    show (OzIntConst target c) = "int_const r" ++ show target ++ ", " ++ show c
    show (OzBoolConst target c) = "int_const r" ++ show target ++ ", " ++ bool_lit c
        where bool_lit True = "1"
              bool_lit False = "0"
    show (OzStringConst target c) = "string_const r" ++ show target ++ ", " ++ show c
    show OzReturn = "return"
    show (OzBinOp op target left right) = getOpStr op ++ " r" ++ show target
        ++ ", r" ++ show left ++ ", r" ++ show right
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
    show (OzUnaryOp op target tmp) = getOpStr op ++ " r" ++ show target
        ++ ", r" ++ show tmp
            where getOpStr Op_negate = "neg_int"
                  getOpStr Op_not    = "not"
    show (OzCallBuiltin f) = "call_builtin " ++ f
    show (OzLoadAddress target from) = "load_address r" ++ show target
        ++ ", " ++ show from
    show (OzBranchOnFalse cond label) = "branch_on_false r" ++ show cond
        ++ ", " ++ label
    show (OzBranchUncond label) = "branch_uncond " ++ label
    show (OzSubOffset target left right) = "sub_offset r" ++ show target
        ++ ", r" ++ show left ++ ", r" ++ show right

generateMachineCode :: Program -> [OzInstruction]
generateMachineCode prog@(Program _ _ procs) =
    [ OzCall "main"
      , OzHalt
    ] ++ concatMap (\x -> generateProcCode x symbolTable initialRegisters) procs
        where symbolTable = getSymbolTable prog

generateProcCode :: Proc -> SymbolTable -> [Int] -> [OzInstruction]
generateProcCode (Proc ident ps _ stmts) global_table rs =
    -- prelude
    [ OzLabel ident
    , OzPushStackFrame stack_size
    ]
    -- load parameters into slots
    ++ (if length ps > 0 then
                         map (\x -> OzStore x x) [0.. length ps - 1]
    else [])
    -- initialise variables
    ++ (if stack_size - length ps > 0 then do
        [OzIntConst 0 0] ++ (map (\x -> OzStore x 0) $ take stack_size [0..])
    else [])
    -- get statement code
    ++ (concatMap (generateStmtCode global_table table) stmts)
    -- cleanup
    ++ [OzPopStackFrame stack_size, OzReturn]
        where (ProcSymbol table stack_size) = findSymbol global_table ident
              labels = [0..]

-- placeholder, idk if this will become part of the symbol table?
getExprType :: Expr -> TypeName
getExprType (BoolLit _) = BoolType
getExprType (IntLit _) = IntType
getExprType (StrLit _) = AliasType ""
getExprType _ = IntType

getLvalAddress :: LValue -> SymbolTable -> [Int] -> [OzInstruction]
getLvalAddress (LId ident) table (r:_) =
    [load_instr r slot]
    where (VarSymbol _ is_ref _ slot) = findSymbol table ident
          load_instr = if is_ref then OzLoad else OzLoadAddress

getLvalAddress (LArray ident expr) table (addr_r:offset_r:rs) =
    [load_instr addr_r slot]
    ++ generateExprCode expr table (offset_r:rs)
    ++ [OzSubOffset addr_r addr_r offset_r]
    where (VarSymbol _ is_ref _ slot) = findSymbol table ident
          load_instr = if is_ref then OzLoad else OzLoadAddress

-- expression, table, registers available for use
generateExprCode :: Expr -> SymbolTable -> [Int] -> [OzInstruction]
generateExprCode (Lval lval) table rs@(val_r:_) =
    getLvalAddress lval table rs
    ++ [OzLoadIndirect val_r val_r]

generateExprCode (BoolLit b) _ (register:_) = [OzBoolConst register b]
generateExprCode (IntLit int) _ (register:_) = [OzIntConst register int]
generateExprCode (StrLit str) _ (register:_) = [OzStringConst register str]

generateExprCode (BinOpExpr op expr1 expr2) table (result_r:left_r:right_r:rs) =
    (generateExprCode expr1 table (left_r:rs))
    ++ (generateExprCode expr2 table (right_r:rs))
    ++ [OzBinOp op result_r left_r right_r]

generateExprCode (PreOpExpr op expr) table (result_r:tmp:rs) =
    generateExprCode expr table (tmp:rs) ++ [OzUnaryOp op result_r tmp]

initialRegisters :: [Int]
initialRegisters = take 1024 [0..]

generateStmtCode :: SymbolTable -> SymbolTable -> Stmt -> [OzInstruction]
generateStmtCode parent table (Assign lval expr) =
    generateExprCode expr table (val_r:rs)
    -- get address
    ++ getLvalAddress lval table (addr_r:rs)
    ++ [OzStoreIndirect addr_r val_r]
    where (val_r:addr_r:rs) = initialRegisters
    

generateStmtCode _ table (Read lval) =
    [OzCallBuiltin readBuiltin]
    ++ getLvalAddress lval table (addr_r:rs)
    ++ [OzStoreIndirect addr_r val_r]
    where (val_r:addr_r:rs) = initialRegisters
          readBuiltin =  case (getExprType (Lval lval)) of
                            BoolType -> "read_bool"
                            IntType  -> "read_int"
                            _ -> "error"


generateStmtCode _ table (Write expr) =
    (generateExprCode expr table initialRegisters)
    ++ [OzCallBuiltin printBuiltin]
    where printBuiltin =  case (getExprType expr) of
                            BoolType -> "print_bool"
                            IntType  -> "print_int"
                            _ -> "print_string"


generateStmtCode _ table (WriteLn expr) =
    (generateExprCode expr table initialRegisters)
    ++ [OzCallBuiltin printBuiltin, OzCallBuiltin "print_newline"]
    where printBuiltin =  case (getExprType expr) of
                            BoolType -> "print_bool"
                            IntType  -> "print_int"
                            _ -> "print_string"

generateStmtCode global_table table (Call ident exprs) =
    -- prepare arguments to be passed
    (concatMap prepareParam args) ++ [OzCall ident]
    where (ProcSymbol arg_map _) = findSymbol global_table ident
          args = [x | (_, x@(VarSymbol _ _ pos _)) <- Map.toList arg_map, pos /= -1]
          prepareParam (VarSymbol _ True _ param_slot) = do
            -- passed in as a ref
            -- is the lvalue in the current context also a ref?
            (Lval (LId lval)) <- return $ exprs !! param_slot
            (VarSymbol _ already_ref _ slot) <- return $ findSymbol table lval
            return $ if already_ref then OzLoad param_slot slot else OzLoadAddress param_slot slot
          prepareParam x@(VarSymbol _ _ r slot) = do
              -- isn't a ref
              generateExprCode (exprs !! r) table $ r:spareRegisters
          spareRegisters = drop numParams initialRegisters
          numParams = length exprs


generateStmtCode global_table table (If expr stmts else_stmts label) =
    (generateExprCode expr table initialRegisters)
    ++ [OzBranchOnFalse 0 $ "label_" ++ label ++ "_0"]
    ++ (concatMap (generateStmtCode global_table table) stmts)
    ++ [OzBranchUncond $ "label_" ++ label ++ "_1",
    OzLabel $ "label_" ++ label ++ "_0"]
    ++ (concatMap (generateStmtCode global_table table) else_stmts)
    ++ [OzLabel $ "label_" ++ label ++ "_1"]


generateStmtCode global_table table (While expr stmts label) =
    [OzLabel $ "label_" ++ label ++ "_0"]
    ++ (generateExprCode expr table initialRegisters)
    ++ [OzBranchOnFalse 0 $ "label_" ++ label ++ "_1"]
    ++ (concatMap (generateStmtCode global_table table) stmts)
    ++ [OzBranchUncond $ "label_" ++ label ++ "_0",
    OzLabel $ "label_" ++ label ++ "_1"]
