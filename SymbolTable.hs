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
    | FieldSymbol TypeName Int
    | TypeSymbol SymbolTable
    deriving Show

type SymbolTable = Map.Map String Symbol

findSymbol :: SymbolTable -> String -> Symbol
findSymbol table ident = table Map.! ident

getSymbolTable :: Program -> SymbolTable
getSymbolTable (Program rs as ps) = symtable
    where symtable = Map.fromList $ concat [proc_syms, record_syms]
          proc_syms = map (getProcSym symtable) ps
          record_syms = map getTypeSym rs
          -- array_syms = map getTypeSym rs

getProcSym :: SymbolTable -> Proc -> (String, Symbol)
getProcSym parent (Proc ident ps vs _) = (ident, ProcSymbol table frame_size)
    where table = Map.fromList $ zipFunction vars [0..]
          vars = (zipWith getParamSyms ps [0..]) ++ (concat $ map getVarSyms vs)
          frame_size = length ps + length vs

zipFunction :: [(a -> b)] -> [a] -> [b]
zipFunction (f:fs) (a:as) = (f a : zipFunction fs as)
zipFunction [] _ = []
zipFunction _ [] = []

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

getTypeSym :: RecordDec -> (String, Symbol)
getTypeSym (RecordDec ident fs) = (ident, TypeSymbol table)
    where table = Map.fromList $ map getFieldSym fs
