-- Skippy, a compiler for the Roo language
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.
--
-- This module builds symbol tables for the entire program, and checks
-- that it is semantically valid. Later stages can assume the programs
-- are well-formed.

module StaticVerify where

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import qualified AST

data SymbolTable = SymbolTable {
                    typeAliases :: Map.Map String RooType,
                    procedures :: Map.Map String Procedure }
                    deriving (Eq, Show)

data Procedure = Procedure {
                    procSymTab :: LocalSymTab,
                    procStackSize :: Int }
                    deriving (Eq, Show)

data LocalSymTab = LocalSymTab (Map.Map String Variable)
    deriving (Eq, Show)

data RooType
    = IntType
    | BoolType
    | StringType
    | ArrayType RooType Integer 
    -- Record elements store their type and position in the record
    | RecordType (Map.Map String (Int, RooType))
    deriving (Eq, Show)

data Variable = Variable {
                    varType :: RooType,
                    varReference :: Bool,
                    varLocation :: Int }
                    deriving (Eq, Show)

data SemanticError
    = DuplicateDefinition Integer Integer
    | ArrayTooSmall Integer Integer

instance Show SemanticError where
    show (DuplicateDefinition _ _) = "Duplicate definition"
    show (ArrayTooSmall _ _) = "Array must have size > 0"

type VerifierMonad = ExceptT SemanticError (State SymbolTable)

emptySymTab :: SymbolTable
emptySymTab = SymbolTable Map.empty Map.empty

symtab :: AST.Program -> Either SemanticError SymbolTable
symtab program = case runState (runExceptT (vrProgram program)) emptySymTab of
                   (Left e, _) -> Left e
                   (Right _, s) -> Right s

vrProgram :: AST.Program -> VerifierMonad ()
vrProgram (AST.Program recordDecs arrayDecs procs) = do
    mapM vrRecord recordDecs
    mapM_ vrArray arrayDecs

vrRecord :: AST.RecordDec -> VerifierMonad ()
vrRecord (AST.RecordDec name fieldDecs) = do
    currentSymTab <- get
    let currentTypeAliases = typeAliases currentSymTab
    when (name `Map.member` currentTypeAliases) (throwError $ DuplicateDefinition 0 0)
    put (currentSymTab {
        typeAliases = Map.insert
                        name
                        (RecordType $ vrFields fieldDecs)
                        currentTypeAliases })

vrFields :: [AST.FieldDec] -> Map.Map String (Int, RooType)
vrFields fields
  = foldl (\m (posn, (AST.FieldDec name rooType)) ->
      if name `Map.member` m
         then errorWithoutStackTrace "Duplicate field"
         else Map.insert name (posn, fieldType rooType) m
    )
    Map.empty
    $ zip [0..] fields

fieldType :: AST.TypeName -> RooType
fieldType AST.BoolType = BoolType
fieldType AST.IntType = IntType
-- Parser grammar guarantees field type is integer or string, so we should
-- never get here.
fieldType _ = error "fieldType: Field is not integer or boolean"

vrArray :: AST.ArrayDec -> VerifierMonad ()
vrArray (AST.ArrayDec name rooType size) = do
    when (size < 1) (throwError $ ArrayTooSmall 0 0)
    currentSymTab <- get
    let currentTypeAliases = typeAliases currentSymTab
    when (name `Map.member` currentTypeAliases) (throwError $ DuplicateDefinition 0 0)
    put (currentSymTab {
        typeAliases = Map.insert
                            name
                            (ArrayType (arrayType currentTypeAliases rooType) size)
                            currentTypeAliases })

arrayType :: Map.Map String RooType -> AST.TypeName -> RooType
arrayType _ AST.BoolType = BoolType
arrayType _ AST.IntType = IntType
arrayType env (AST.AliasType name)
  = case Map.lookup name env of
      Just r@(RecordType _) -> r
      _ -> errorWithoutStackTrace "Bad array type"

{-
getSymTab :: AST.Program -> GlobalSymTab
getSymTab ast = GlobalSymTab (aliases ast) (procedures ast)

trType :: AST.TypeName -> Datatype
trType AST.BoolType = BoolType
trType AST.IntType = IntType

aliases :: AST.Program -> Map.Map String Datatype
aliases (AST.Program recs arrs _)
  = foldl
        Map.empty
        (\m (k, v) -> Map.insertWithKey duplicateEntry k v m)
        ((map records recs) ++ (map arrays arrs))

records :: [AST.RecordDec] -> [Datatype]
records [] = []
records ((AST.RecordDec name f):xs)
  = (name, RecordType (fieldDecs f)):(records xs)

field :: [AST.FieldDec] -> Map.Map String (Int, Datatype)
field f
  = foldl
        Map.empty
        (\m (k, v) -> Map.insertWithKey duplicateEntry k v m)
        (map id (zip [0..] f))

arrays :: [AST.ArrayDec] -> [(String, Datatype)]
arrays [] = []
arrays ((AST.ArrayDec name rooType size):xs)
  = (name, ArrayType (trType rooType) size):(arrays xs)

procedures :: AST.Program -> Map.Map String Procedure
procedures _ = Map.empty
-}

duplicateEntry :: String -> a -> a -> String
duplicateEntry key _ _
  = errorWithoutStackTrace "Duplicate definition of " ++ key

semanticError = errorWithoutStackTrace
