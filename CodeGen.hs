-- Skippy, a compiler for the Roo language
--
-- Submitted for assignment 3 of COMP90045, 2020
-- By Luke Ceddia [lceddia] and Ben Harper [bharper1]
-- 28 October 2020
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.
--
-- This module is responsible for generating Oz assembly. It assumed the
-- provided symbol table and AST represent a semantically correct program
-- and thus does not do error checking.

-- About calling conventions & register allocation:
--  * Procedures are called by passing parameters left to right in registers
--    r0 though to rN as needed.
--  * Procedures write arguments out to allocated stack slots immediately
--    after initialising their stack frame.
--  * No registers are preserved between Roo statements. This allows each
--    statement to use registers destructively from r0 upwards.
--  * This isn't particularly efficient but it's simple and it works.

module CodeGen where

import qualified AST as AST
import SymbolTable
import SemanticCheck (exprType)
import qualified Data.Map.Lazy as Map
import Data.List (intercalate)

-- Each data constructor's arguments correspond exactly to the Oz assembly
-- instruction except where noted.
data OzInstruction
    = OzCall String
    | OzLabel String
    | OzPrint AST.TypeName -- call_builtin of appropriately typed print
    | OzHalt
    | OzPushStackFrame Int
    | OzPopStackFrame Int
    | OzStore Int Int
    | OzStoreIndirect Int Int
    | OzLoad Int Int
    | OzLoadIndirect Int Int
    | OzIntConst Int Int
    | OzBoolConst Int Bool -- Ultimately translated to an int_const
    | OzStringConst Int String
    | OzReturn
    | OzBinOp AST.BinOp Int Int Int -- Selects appropriate operator
    | OzUnaryOp AST.PreOp Int Int -- Selects appropriate operator
    | OzCallBuiltin String
    | OzLoadAddress Int Int
    | OzBranchOnFalse Int String
    | OzBranchUncond String
    | OzSubOffset Int Int Int

-- Straight-forward string equivalents of above instructions
instance Show OzInstruction where
    show (OzCall p)
      = "call " ++ p
    show (OzLabel l)
      = l ++ ":"
    show (OzPrint t)
      = "call_builtin " ++ printBuiltin
        where printBuiltin = case t of
                                 AST.BoolType -> "print_bool"
                                 AST.IntType  -> "print_int"
                                 _ -> "print_string"
    show OzHalt
      = "halt"
    show (OzPushStackFrame x)
      = "push_stack_frame " ++ show x
    show (OzPopStackFrame x)
      = "pop_stack_frame " ++ show x
    show (OzStore target from)
      = "store " ++ show target ++ ", r" ++ show from
    show (OzStoreIndirect target from)
      = "store_indirect r" ++ show target ++ ", r" ++ show from
    show (OzLoad target from)
      = "load r" ++ show target ++ ", " ++ show from
    show (OzLoadIndirect target from)
      = "load_indirect r" ++ show target ++ ", r" ++ show from
    show (OzIntConst target c)
      = "int_const r" ++ show target ++ ", " ++ show c
    show (OzBoolConst target c)
      = "int_const r" ++ show target ++ ", " ++ bool_lit c
        where bool_lit True = "1"
              bool_lit False = "0"
    show (OzStringConst target c)
      = "string_const r" ++ show target ++ ", \"" ++ c ++ "\""
    show OzReturn
      = "return"
    show (OzBinOp op target left right)
      = getOpStr op ++ " r" ++ show target
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
    show (OzUnaryOp op target tmp)
      = getOpStr op ++ " r" ++ show target ++ ", r" ++ show tmp
            where getOpStr AST.Op_negate = "neg_int"
                  getOpStr AST.Op_not    = "not"
    show (OzCallBuiltin f)
      = "call_builtin " ++ f
    show (OzLoadAddress target from)
      = "load_address r" ++ show target ++ ", " ++ show from
    show (OzBranchOnFalse cond label)
      = "branch_on_false r" ++ show cond ++ ", " ++ label
    show (OzBranchUncond label)
      = "branch_uncond " ++ label
    show (OzSubOffset target left right)
      = "sub_offset r" ++ show target
        ++ ", r" ++ show left ++ ", r" ++ show right

-- Generate labels that are guaranteed unique by using the object's
-- source code position. This sounds janky but ends up working quite well,
-- without the need for more state.
getLabel :: AST.Posn -> String
getLabel (x, y) = "label_" ++ show x ++ "_" ++ show y

-- The full set of registers available. Various functions will reserve some
-- elements and pass a tail to children to control register allocation.
initialRegisters :: [Int]
initialRegisters = [0..1023]

-- Main entry point for this module. Generate list of Oz instructions from
-- symbol table and AST.
generateMachineCode :: SymbolTable -> AST.Program -> [OzInstruction]
generateMachineCode table prog@(AST.Program _ _ procs) =
    [ OzCall "main"
    , OzHalt
    ] ++ concatMap (\x -> generateProcCode table x initialRegisters) procs

-- Generate list of Oz instructions beginning with label for a procedure.
generateProcCode :: SymbolTable -> AST.Proc -> [Int] -> [OzInstruction]
generateProcCode table (AST.Proc _ ident ps _ stmts) rs =
    -- prelude
    [ OzLabel ident
    , OzPushStackFrame stack_size
    ]
    -- write passed parameters out to slots
    ++ map (\x -> OzStore x x) [0..length ps - 1]
    -- initialise all local variables to 0
    ++ (if local_size > 0 then
            [OzIntConst 0 0]
            ++ map (\x -> OzStore x 0) (take local_size [length ps..])
       else [])
    -- get statement code
    ++ concatMap (generateStmtCode table locals) stmts
    -- cleanup
    ++ [OzPopStackFrame stack_size, OzReturn]
        where (Procedure _ locals stack_size) = getProc table ident
              local_size = stack_size - length ps

-- Generate instructions to load address of an lvalue into next available
-- register.
typeSize :: RooType -> Int
typeSize (RecordType fields) = Map.size fields
typeSize _ = 1

getLvalAddress :: Locals -> AST.LValue -> [Int] -> [OzInstruction]

-- Simple variable access
getLvalAddress locals (AST.LId _ ident) (r:_) =
    [load_instr r slot]
        where (Variable _ is_ref slot) = getLocal locals ident
              load_instr = if is_ref then OzLoad else OzLoadAddress

-- Record field access
getLvalAddress locals (AST.LField _ var_ident field_ident) (addr_r:offset_r:_) =
    -- Load address of record
    [load_instr addr_r slot]
    -- Apply offset for field access
    ++ [OzIntConst offset_r offset]
    ++ [OzSubOffset addr_r addr_r offset_r]
        where (Variable (RecordType fields) is_ref slot)
                = getLocal locals var_ident
              (offset, _) = fields Map.! field_ident
              load_instr = if is_ref then OzLoad else OzLoadAddress

-- Array element
getLvalAddress locals (AST.LArray _ ident expr) (addr_r:offset_r:size_r:rs) =
    -- Load address of first array element
    [load_instr addr_r slot]
    -- Compute array index
    ++ generateExprCode locals expr (offset_r:rs)
    -- Apply index
    ++ if type_size == 1
          then []
          else [OzIntConst size_r type_size
               , OzBinOp AST.Op_mult offset_r offset_r size_r]
    ++ [OzSubOffset addr_r addr_r offset_r]
    where (Variable (ArrayType arr_type _) is_ref slot) = getLocal locals ident
          load_instr = if is_ref then OzLoad else OzLoadAddress
          type_size = typeSize arr_type

-- Combination of array and record
getLvalAddress locals
               (AST.LArrayField _ var_ident expr field_ident)
               (addr_r:offset_r:field_offset_r:rs) =
    -- Array base
    [load_instr addr_r slot]
    -- Array index computation
    ++ generateExprCode locals expr (offset_r:rs)
    -- multiply array offset by the field size
    ++ [ OzIntConst field_offset_r record_size
       , OzBinOp AST.Op_mult offset_r offset_r field_offset_r
       -- add the field offset
       , OzIntConst field_offset_r field_offset
       , OzBinOp AST.Op_plus offset_r offset_r field_offset_r
       ]
    ++ [OzSubOffset addr_r addr_r offset_r]
    where (Variable (ArrayType (RecordType fields) _) is_ref slot)
              = getLocal locals var_ident
          (field_offset,_) = fields Map.! field_ident
          record_size = Map.size fields
          load_instr = if is_ref then OzLoad else OzLoadAddress

-- Generate code to evaluate an expression, leaving result in next register.
-- Arguments: local symbols, expression, available registers
generateExprCode :: Locals -> AST.Expr -> [Int] -> [OzInstruction]

-- lvalue is straight-forward
generateExprCode table (AST.Lval _ lval) rs@(val_r:_)
  = getLvalAddress table lval rs
    ++ [OzLoadIndirect val_r val_r]

-- Literals are also easy
generateExprCode _ (AST.BoolLit _ b) (register:_)
  = [OzBoolConst register b]
generateExprCode _ (AST.IntLit _ int) (register:_)
  = [OzIntConst register int]
generateExprCode _ (AST.StrLit _ str) (register:_)
  = [OzStringConst register str]

-- All binary operations are the same and map directly to Oz commands.
-- Evaluate left, evaluate right, do operation.
generateExprCode table (AST.BinOpExpr _ op expr1 expr2) (result_r:right_r:rs)
  = (generateExprCode table expr1 (result_r:rs))
    ++ (generateExprCode table expr2 (right_r:rs))
    ++ [OzBinOp op result_r result_r right_r]

-- Prefix unary operators are even easier.
generateExprCode table (AST.PreOpExpr _ op expr) (result_r:tmp:rs)
  = generateExprCode table expr (tmp:rs) ++ [OzUnaryOp op result_r tmp]

-- Generate code for each of the Roo statements
generateStmtCode :: SymbolTable -> Locals -> AST.Stmt -> [OzInstruction]

-- Assignment of both scalars and compound types
generateStmtCode table locals (AST.Assign _ lval expr)
  -- scalar booleans and integers are simple
  | isPrimitive lval = generateExprCode locals expr (val_r:rs)
        -- get address
        ++ getLvalAddress locals lval (addr_r:rs)
        ++ [OzStoreIndirect addr_r val_r]
  | otherwise = -- array or record, deeeeeep copy time
      getLvalAddress locals lval (lval_addr:rs)
      ++ getLvalAddress locals (rval expr) (rval_addr:rs)
      ++ (concat $ take (1 + copySize lval) $ repeat
          [ OzLoadIndirect tmp_r rval_addr
          , OzStoreIndirect lval_addr tmp_r
          , OzIntConst tmp_r 1
          , OzSubOffset lval_addr lval_addr tmp_r
          , OzSubOffset rval_addr rval_addr tmp_r
          ])
    where (val_r:addr_r:rs) = initialRegisters
          (lval_addr:rval_addr:tmp_r:_) = initialRegisters
          isPrimitive (AST.LId _ lval_id) = not . isAlias $ getvar lval_id
          isPrimitive (AST.LArray _ lval_id _) = not . isArrOfRec $ getvar lval_id
          isPrimitive _ = True
          isAlias (Variable (ArrayType _ _) _ _) = True
          isAlias (Variable (RecordType _) _ _) = True
          isAlias _ = False
          isArrOfRec (Variable (ArrayType (RecordType _) _) _ _) = True
          isArrOfRec _ = False
          getvar x = getLocal locals x
          copySize (AST.LId _ lval_id) = variableSize $ getvar lval_id
          copySize (AST.LArrayField _ _ _ _) = 1
          copySize (AST.LArray _ lval_id _) = variableSize $ getvar lval_id
          rval (AST.Lval _ x) = x

-- Read command for all types
generateStmtCode _ locals (AST.Read pos lval) =
    -- writes to r0
    [OzCallBuiltin readBuiltin]
    -- store value in final location
    ++ getLvalAddress locals lval (addr_r:rs)
    ++ [OzStoreIndirect addr_r val_r]
    where (val_r:addr_r:rs) = initialRegisters
          readBuiltin = case (exprType locals (AST.Lval pos lval)) of
                          (Right BoolType) -> "read_bool"
                          (Right IntType)  -> "read_int"
                          _                -> "error"

-- Write command for all types
generateStmtCode _ locals (AST.Write _ expr) =
    -- Evaluate expression
    (generateExprCode locals expr initialRegisters)
    -- Value to write in r0
    ++ [OzCallBuiltin printBuiltin]
    where printBuiltin = case (exprType locals expr) of
                           (Right BoolType) -> "print_bool"
                           (Right IntType)  -> "print_int"
                           _                -> "print_string"

-- WriteLn command for all types
-- As per Write but with a print_newline
generateStmtCode _ locals (AST.WriteLn _ expr) =
    (generateExprCode locals expr initialRegisters)
    ++ [OzCallBuiltin printBuiltin, OzCallBuiltin "print_newline"]
    where printBuiltin =  case (exprType locals expr) of
                           (Right BoolType) -> "print_bool"
                           (Right IntType)  -> "print_int"
                           _                -> "print_string"

-- Call command
generateStmtCode table locals (AST.Call _ ident exprs) =
    -- Prepare arguments to be passed
    (concatMap prepareParam (zip [0..] args))
    -- Make call
    ++ [OzCall ident]
    where (Procedure args _ _) = getProc table ident
          prepareParam (arg_idx, Variable _ True _) = do
              -- passed in as a ref
              (AST.Lval _ lval) <- return $ exprs !! arg_idx
              getLvalAddress locals lval (arg_idx:rs)
          prepareParam x@(arg_idx, _) = do
              -- isn't a ref
              generateExprCode locals (exprs !! arg_idx) $ arg_idx:rs
          rs = drop numParams initialRegisters
          numParams = length exprs

-- If conditional
generateStmtCode table locals (AST.If pos expr stmts else_stmts) =
    -- Evaluate conditional
    (generateExprCode locals expr initialRegisters)
    -- Jump to false branch
    ++ [OzBranchOnFalse 0 $ label ++ "_0"]
    -- Generate true branch
    ++ (concatMap (generateStmtCode table locals) stmts)
    -- Jump to end
    ++ [OzBranchUncond $ label ++ "_1",
    -- False branch
    OzLabel $ label ++ "_0"]
    ++ (concatMap (generateStmtCode table locals) else_stmts)
    ++ [OzLabel $ label ++ "_1"]
        where label = getLabel pos

-- While loop
generateStmtCode table locals (AST.While pos expr stmts) =
    [OzLabel $ label ++ "_0"]
    -- Evaluate guard
    ++ (generateExprCode locals expr initialRegisters)
    -- Exit jump
    ++ [OzBranchOnFalse 0 $ label ++ "_1"]
    -- Body
    ++ (concatMap (generateStmtCode table locals) stmts)
    -- Jump to top
    ++ [OzBranchUncond $ label ++ "_0",
    OzLabel $ label ++ "_1"]
        where label = getLabel pos
