-- Skippy, a compiler for the Roo language
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.

module SymbolTable where

import AST
import qualified Data.Map.Lazy as Map

data Symbol
    = ProcSymbol
        SymbolTable -- variables
        Int -- stack frame size
    | VarSymbol TypeName Bool Int Int
    -- is reference, parameter position (-1 for variable), location
    | ArraySymbol TypeName Int
    | FieldSymbol TypeName Int
    | RecordSymbol Int SymbolTable -- size, parameters
    deriving Show

type SymbolTable = Map.Map String Symbol

findSymbol :: SymbolTable -> String -> Symbol
findSymbol table ident = table Map.! ident

getSymbolTable :: Program -> SymbolTable
getSymbolTable (Program rs as ps) = symtable
    where symtable = Map.fromList $ concat [proc_syms, record_syms, array_syms]
          proc_syms = map (getProcSym symtable) ps
          record_syms = map getRecordSym rs
          array_syms = map getArraySym as

getProcSym :: SymbolTable -> Proc -> (String, Symbol)
getProcSym parent (Proc ident ps vs _) = (ident, ProcSymbol table frame_size)
    where table = Map.fromList $ vars'
          vars = (zipWith getParamSyms ps [0..]) ++ (concat $ map getVarSyms vs)
          vars' = assignSlots parent table 0 vars
          frame_size = (sum $ map (\(ident, _) -> getVarSize parent table ident) vars')

assignSlots :: SymbolTable -> SymbolTable -> Int -> [Int -> (String, Symbol)] -> [(String, Symbol)]
assignSlots _ _ _ [] = []
assignSlots parent table start (v:vs) = v':vs'
    where v'@(ident, _) = v start
          vs' = assignSlots parent table next vs
          next = start + (getVarSize parent table ident)

getVarSize :: SymbolTable -> SymbolTable -> String -> Int
getVarSize parent table ident = getTypeSize parent typename
    where (VarSymbol typename _ _ _) = findSymbol table ident

getTypeSize :: SymbolTable -> TypeName -> Int
getTypeSize table (AliasType ident) = size
    where (ArraySymbol _ size) = findSymbol table ident
getTypeSize _ _ = 1

getVarSyms :: VarDec -> [Int -> (String, Symbol)]
getVarSyms (VarDec idents typename) = map f idents
    where f x = getVarSym x typename False $ -1

getVarSym :: String -> TypeName -> Bool -> Int -> Int -> (String, Symbol)
getVarSym ident typename is_ref is_param slot
  = (ident, VarSymbol typename is_ref is_param slot)

getParamSyms :: Parameter -> Int -> Int -> (String, Symbol)
getParamSyms (RefParam ident typename) pos = getVarSym ident typename True pos
getParamSyms (ValParam ident typename) pos = getVarSym ident typename False pos

getFieldSym :: FieldDec -> (String, Symbol)
getFieldSym (FieldDec ident typename) = (ident, FieldSymbol typename 0)

getRecordSym :: RecordDec -> (String, Symbol)
getRecordSym (RecordDec ident fs) = (ident, RecordSymbol 0 table)
    where table = Map.fromList $ map getFieldSym fs

getArraySym :: ArrayDec -> (String, Symbol)
getArraySym (ArrayDec ident typename size) = (ident, ArraySymbol typename $ fromInteger size)
