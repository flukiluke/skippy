-- Skippy, a compiler for the Roo language
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.

module StaticVerify where

import qualified Data.Map.Strict as Map
import AST

data GlobalSymTab = GlobalSymTab {
                            typeAliases :: Map.Map String Datatype,
                            procs :: Map.Map String Procedure }

data Procedure = Procedure {
                    procSymTab :: LocalSymTab,
                    procStackSize :: Int }

data LocalSymTab = LocalSymTab (Map.Map String Variable)

data Datatype
    = IntType
    | BoolType
    | StringType
    | ArrayType Datatype Integer 
    -- Record elements store their type and position in the record
    | RecordType (Map.Map String (Int, Datatype))

data Variable = Variable {
                    varType :: Datatype,
                    varReference :: Bool,
                    varLocation :: Int }

getSymTab :: Program -> GlobalSymTab
getSymTab ast = GlobalSymTab (aliases ast) (procedures ast)

aliases :: Program -> Map.Map String Datatype
aliases (Program recs arrs _)
  = foldl
        Map.empty
        (\m (k, v) -> Map.insertWithKey duplicateEntry k v m)
        ((map records recs) ++ (map arrays arrs))

records :: [RecordDec] -> [Datatype]
records [] = []
records ((RecordDec name f):xs)
  = (name, RecordType (fieldDecs f)):(records xs)

field :: [FieldDec] -> Map.Map String (Int, Datatype)
field f
  = foldl
        Map.empty
        (\m (k, v) -> Map.insertWithKey duplicateEntry k v m)
        (map id (zip [0..] f))

arrays :: [ArrayDec] -> [Datatype]
arrays [] = []
arrays ((ArrayDec name typeName size):xs)
  = (name, ArrayType (primitiveType typeName) size):(arrays xs)

procedures :: Program -> Map.Map String Procedure
procedures _ = error

duplicateEntry key _ _
  = errorWithoutStackTrace "Duplicate definition of " ++ key
