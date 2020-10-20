-- Skippy, a compiler for the Roo language
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.

module CodeGen where

import qualified AST as AST
import SymbolTable
import SemanticCheck (exprType)
import qualified Data.Map.Lazy as Map
import Data.List (intercalate)

data OzInstruction
    = OzCall String
    | OzLabel String
    | OzPrint AST.TypeName
    | OzHalt
    | OzPushStackFrame Int
    | OzPopStackFrame Int
    | OzStore Int Int
    | OzStoreIndirect Int Int
    | OzLoad Int Int
    | OzLoadIndirect Int Int
    | OzIntConst Int Int
    | OzBoolConst Int Bool
    | OzStringConst Int String
    | OzReturn
    | OzBinOp AST.BinOp Int Int Int
    | OzUnaryOp AST.PreOp Int Int
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
                                 AST.BoolType -> "print_bool"
                                 AST.IntType  -> "print_int"
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
            where getOpStr (AST.Op_or)    = "or"
                  getOpStr (AST.Op_and)   = "and"
                  getOpStr AST.Op_eq      = "cmp_eq_int"
                  getOpStr AST.Op_neq     = "cmp_ne_int"
                  getOpStr AST.Op_lt      = "cmp_lt_int"
                  getOpStr AST.Op_lteq    = "cmp_le_int"
                  getOpStr AST.Op_gt      = "cmp_gt_int"
                  getOpStr AST.Op_gteq    = "cmp_ge_int"
                  getOpStr AST.Op_plus    = "add_int"
                  getOpStr AST.Op_minus   = "sub_int"
                  getOpStr AST.Op_mult    = "mul_int"
                  getOpStr AST.Op_divide  = "div_int"
    show (OzUnaryOp op target tmp) = getOpStr op ++ " r" ++ show target
        ++ ", r" ++ show tmp
            where getOpStr AST.Op_negate = "neg_int"
                  getOpStr AST.Op_not    = "not"
    show (OzCallBuiltin f) = "call_builtin " ++ f
    show (OzLoadAddress target from) = "load_address r" ++ show target
        ++ ", " ++ show from
    show (OzBranchOnFalse cond label) = "branch_on_false r" ++ show cond
        ++ ", " ++ label
    show (OzBranchUncond label) = "branch_uncond " ++ label
    show (OzSubOffset target left right) = "sub_offset r" ++ show target
        ++ ", r" ++ show left ++ ", r" ++ show right

getLabel :: AST.Posn -> String
getLabel (x, y) = "label_" ++ show x ++ "_" ++ show y

generateMachineCode :: SymbolTable -> AST.Program -> [OzInstruction]
generateMachineCode table prog@(AST.Program _ _ procs) =
    [ OzCall "main"
      , OzHalt
    ] ++ concatMap (\x -> generateProcCode table x initialRegisters) procs

generateProcCode :: SymbolTable -> AST.Proc -> [Int] -> [OzInstruction]
generateProcCode table (AST.Proc _ ident ps _ stmts) rs =
    -- prelude
    [ OzLabel ident
    , OzPushStackFrame stack_size
    ]
    -- load parameters into slots
    ++ (if length ps > 0 then map (\x -> OzStore x x) [0.. length ps - 1]
    else [])
    -- initialise variables
    ++ (if stack_size - length ps > 0 then do
        [OzIntConst 0 0] ++ (map (\x -> OzStore x 0) $ take stack_size [0..])
    else [])
    -- get statement code
    ++ (concatMap (generateStmtCode table locals) stmts)
    -- cleanup
    ++ [OzPopStackFrame stack_size, OzReturn]
        where (Procedure _ locals stack_size) = getProc table ident
              labels = [0..]

getLvalAddress :: Locals -> AST.LValue -> [Int] -> [OzInstruction]
getLvalAddress locals (AST.LId _ ident) (r:_) =
    [load_instr r slot]
    where (Variable _ is_ref slot) = getLocal locals ident
          load_instr = if is_ref then OzLoad else OzLoadAddress

getLvalAddress locals (AST.LArray _ ident expr) (addr_r:offset_r:rs) =
    [load_instr addr_r slot]
    ++ generateExprCode locals expr (offset_r:rs)
    ++ [OzSubOffset addr_r addr_r offset_r]
    where (Variable _ is_ref slot) = getLocal locals ident
          load_instr = if is_ref then OzLoad else OzLoadAddress

-- expression, table, registers available for use
generateExprCode :: Locals -> AST.Expr -> [Int] -> [OzInstruction]
generateExprCode table (AST.Lval _ lval) rs@(val_r:_) =
    getLvalAddress table lval rs
    ++ [OzLoadIndirect val_r val_r]

generateExprCode _ (AST.BoolLit _ b) (register:_) = [OzBoolConst register b]
generateExprCode _ (AST.IntLit _ int) (register:_) = [OzIntConst register int]
generateExprCode _ (AST.StrLit _ str) (register:_) = [OzStringConst register str]

generateExprCode table (AST.BinOpExpr _ op expr1 expr2) (result_r:right_r:rs) =
    (generateExprCode table expr1 (result_r:rs))
    ++ (generateExprCode table expr2 (right_r:rs))
    ++ [OzBinOp op result_r result_r right_r]

generateExprCode table (AST.PreOpExpr _ op expr) (result_r:tmp:rs) =
    generateExprCode table expr (tmp:rs) ++ [OzUnaryOp op result_r tmp]

initialRegisters :: [Int]
initialRegisters = take 1024 [0..]

generateStmtCode :: SymbolTable -> Locals -> AST.Stmt -> [OzInstruction]
generateStmtCode table locals (AST.Assign _ lval expr) =
    generateExprCode locals expr (val_r:rs)
    -- get address
    ++ getLvalAddress locals lval (addr_r:rs)
    ++ [OzStoreIndirect addr_r val_r]
    where (val_r:addr_r:rs) = initialRegisters

generateStmtCode _ locals (AST.Read pos lval) =
    [OzCallBuiltin readBuiltin]
    ++ getLvalAddress locals lval (addr_r:rs)
    ++ [OzStoreIndirect addr_r val_r]
    where (val_r:addr_r:rs) = initialRegisters
          readBuiltin = case (exprType locals (AST.Lval pos lval)) of
                          (Right BoolType) -> "read_bool"
                          (Right IntType)  -> "read_int"
                          _                -> "error"

generateStmtCode _ locals (AST.Write _ expr) =
    (generateExprCode locals expr initialRegisters)
    ++ [OzCallBuiltin printBuiltin]
    where printBuiltin = case (exprType locals expr) of
                           (Right BoolType) -> "print_bool"
                           (Right IntType)  -> "print_int"
                           _                -> "print_string"

generateStmtCode _ locals (AST.WriteLn _ expr) =
    (generateExprCode locals expr initialRegisters)
    ++ [OzCallBuiltin printBuiltin, OzCallBuiltin "print_newline"]
    where printBuiltin =  case (exprType locals expr) of
                           (Right BoolType) -> "print_bool"
                           (Right IntType)  -> "print_int"
                           _                -> "print_string"

generateStmtCode table locals (AST.Call _ ident exprs) =
    -- prepare arguments to be passed
    (concatMap prepareParam (zip [1..] args)) ++ [OzCall ident]
    where (Procedure args _ _) = getProc table ident
          prepareParam (arg_idx, Variable _ True _) = do
            -- passed in as a ref
            -- is the lvalue in the current context also a ref?
            (AST.Lval _ (AST.LId _ lval)) <- return $ exprs !! arg_idx
            (Variable _ already_ref slot) <- return $ getLocal locals lval
            return $ if already_ref then OzLoad arg_idx slot else OzLoadAddress arg_idx slot
          prepareParam x@(arg_idx, _) = do
              -- isn't a ref
              generateExprCode locals (exprs !! arg_idx) $ arg_idx:spareRegisters
          spareRegisters = drop numParams initialRegisters
          numParams = length exprs

generateStmtCode table locals (AST.If pos expr stmts else_stmts) =
    (generateExprCode locals expr initialRegisters)
    ++ [OzBranchOnFalse 0 $ label ++ "_0"]
    ++ (concatMap (generateStmtCode table locals) stmts)
    ++ [OzBranchUncond $ label ++ "_1",
    OzLabel $ label ++ "_0"]
    ++ (concatMap (generateStmtCode table locals) else_stmts)
    ++ [OzLabel $ label ++ "_1"]
        where label = getLabel pos

generateStmtCode table locals (AST.While pos expr stmts) =
    [OzLabel $ label ++ "_0"]
    ++ (generateExprCode locals expr initialRegisters)
    ++ [OzBranchOnFalse 0 $ label ++ "_1"]
    ++ (concatMap (generateStmtCode table locals) stmts)
    ++ [OzBranchUncond $ label ++ "_0",
    OzLabel $ label ++ "_1"]
        where label = getLabel pos
