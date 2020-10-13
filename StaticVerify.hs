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
    | BadArrayType Integer Integer

instance Show SemanticError where
    show (DuplicateDefinition _ _) = "Duplicate definition"
    show (ArrayTooSmall _ _) = "Array must have size > 0"
    show (BadArrayType _ _) = "Array type must be integer, boolean or a record"

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
    types <- gets typeAliases
    fields <- vrFields fieldDecs
    when (name `Map.member` types) (throwError $ DuplicateDefinition 0 0)
    modify (\s -> s { typeAliases = Map.insert name (RecordType fields) types })

vrFields :: [AST.FieldDec] -> VerifierMonad (Map.Map String (Int, RooType))
vrFields fields
  = foldM (\m (posn, (AST.FieldDec name rooType)) ->
      if name `Map.member` m
         then throwError (DuplicateDefinition 0 0)
         else return (Map.insert name (posn, fieldType rooType) m)
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
    types <- gets typeAliases
    elementType <- arrayType rooType
    when (name `Map.member` types) (throwError $ DuplicateDefinition 0 0)
    modify (\s -> s { typeAliases = Map.insert name (elementType size) types })

-- Note: partially applied data constructor so size can be added by caller
arrayType :: AST.TypeName -> VerifierMonad (Integer -> RooType)
arrayType AST.BoolType = return $ ArrayType BoolType
arrayType AST.IntType = return $ ArrayType IntType
arrayType (AST.AliasType name) = do
    types <- gets typeAliases
    case Map.lookup name types of
        Just r@(RecordType _) -> return $ ArrayType r
        _ -> throwError $ BadArrayType 0 0
