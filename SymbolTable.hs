-- Skippy, a compiler for the Roo language
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.

module SymbolTable where

import AST
import qualified Data.Map.Lazy as Map

data Symbol = Symbol
    { location :: Int
    , symTable :: Maybe SymbolTable
    , symType :: Maybe TypeName
    }

type SymbolTable = Map.Map String Symbol

getSymbolTable :: Program -> SymbolTable
getSymbolTable (Program rs as ps) = symtable
    where symtable = Map.fromList syms
          syms = map getProcSym ps

getProcSym :: Proc -> (String, Symbol)
getProcSym (Proc ident ps vs _) = (ident, Symbol loc symTable Nothing)
    where loc = 0
          symTable = Just . Map.fromList . concat $ map getVarSyms vs

getVarSyms :: VarDec -> [(String, Symbol)]
getVarSyms (VarDec idents typename) = map getVarSym idents
    where loc = 0
          getVarSym = (\x -> (x, Symbol loc Nothing (Just typename)))
