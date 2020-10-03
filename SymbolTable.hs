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
    | VarSymbol TypeName Int -- location
    | FieldSymbol TypeName Int
    | TypeSymbol SymbolTable

type SymbolTable = Map.Map String Symbol

getSymbolTable :: Program -> SymbolTable
getSymbolTable (Program rs as ps) = symtable
    where symtable = Map.fromList $ concat [proc_syms, record_syms]
          proc_syms = map getProcSym ps
          record_syms = map getTypeSym rs
          -- array_syms = map getTypeSym rs

getProcSym :: Proc -> (String, Symbol)
getProcSym (Proc ident _ vs _) = (ident, ProcSymbol table frame_size)
    where table = Map.fromList . concat $ zipFunction (map getVarSyms vs) [0..]
          frame_size = length vs

zipFunction :: [(a -> b)] -> [a] -> [b]
zipFunction (f:fs) (a:as) = (f a : zipFunction fs as)
zipFunction [] _ = []
zipFunction _ [] = []

getVarSyms :: VarDec -> Int -> [(String, Symbol)]
getVarSyms (VarDec idents typename) slot = map getVarSym idents
    where loc = 0
          getVarSym = (\x -> (x, VarSymbol typename slot))

getFieldSym :: FieldDec -> (String, Symbol)
getFieldSym (FieldDec ident typename) = (ident, FieldSymbol typename 0)

getTypeSym :: RecordDec -> (String, Symbol)
getTypeSym (RecordDec ident fs) = (ident, TypeSymbol table)
    where table = Map.fromList $ map getFieldSym fs
