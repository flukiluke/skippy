-- Skippy, a compiler for the Roo language
--
-- Submitted for assignment 3 of COMP90045, 2020
-- By Luke Ceddia [lceddia] and Ben Harper [bharper1]
-- 28 October 2020
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.
--
-- This module builds a symbol table for the entire program. It also performs
-- some semantic checks where convenient: it checks uniqueness of names and
-- array sizes.

module SymbolTable where

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Except
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import qualified AST
import ErrorHandling

type ProcSig = [RooType]

-- All variables available in a procedure, including formal arguments
type Locals = Map.Map String Variable

-- "Global" symbols, need to be in scope everywhere
data SymbolTable = SymbolTable {
                    typeAliases :: Map.Map String RooType,
                    procedures :: Map.Map String Procedure }
                    deriving (Eq, Show)

-- Data for a single procedure
data Procedure = Procedure {
                    procSig :: [Variable], -- List of formal arguments
                    procSymTab :: Locals, -- Local variables
                    procStackSize :: Int } -- We calculate required stack size
                    deriving (Eq, Show)

data RooType
    = IntType
    | BoolType
    | StringType
    -- Element type and number of elements
    | ArrayType RooType Int
    -- Record fields store their type and position in the record
    | RecordType (Map.Map String (Int, RooType))
    deriving (Eq, Show)

data Variable = Variable {
                    -- Type as per above
                    varType :: RooType,
                    -- Is this an argument being passed without 'val'?
                    varByRef :: Bool,
                    -- We allocate stack slots sequentially
                    varStackSlot :: Int }
                    deriving (Eq, Show)

-- Use a state monad to hold the in progress symbol table, with Either
-- to signal an error.
type SymTabMonad = ExceptT SemanticError (State SymbolTable)

emptySymTab :: SymbolTable
emptySymTab = SymbolTable Map.empty Map.empty

-- Top-level entry point. Generate a symbol table for an AST.
makeSymtab :: AST.Program -> Either SemanticError SymbolTable
makeSymtab program = case runState (runExceptT (stProgram program)) emptySymTab of
                   (Left e, _) -> Left e
                   (Right _, s) -> Right s

-- These get* functions are provided for convenience of looking up things
-- in the symbol table.
getProc :: SymbolTable -> String -> Procedure
getProc symbolTable procName
  = (procedures symbolTable) Map.! procName

getLocal :: Locals -> String -> Variable
getLocal locals name
  = locals Map.! name

-- Note: st- prefix just means symbol table. Used to avoid confusing name
-- clashes.

-- Generate symbols for all program components
stProgram :: AST.Program -> SymTabMonad ()
stProgram (AST.Program recordDecs arrayDecs procs) = do
    mapM stRecord recordDecs
    mapM stArray arrayDecs
    mapM_ stProcedure procs

-- Generate symbols for a record, checking for duplicate record name
stRecord :: AST.RecordDec -> SymTabMonad ()
stRecord (AST.RecordDec posn name fieldDecs) = do
    currentSymTab <- get
    types <- gets typeAliases
    fields <- stFields fieldDecs
    when (name `Map.member` types) (throwError $ DuplicateDefinition posn)
    modify (\s -> s { typeAliases = Map.insert name (RecordType fields) types })

-- Generate symbols for fields of a record, checking for repeated field names
stFields :: [AST.FieldDec] -> SymTabMonad (Map.Map String (Int, RooType))
stFields fields
  = foldM (\m (index, (AST.FieldDec posn name rooType)) ->
      if name `Map.member` m
         then throwError (DuplicateDefinition posn)
         else return (Map.insert name (index, fieldType rooType) m)
    )
    Map.empty
    -- zip with [0..] so each field is numbered with offset in the record.
    -- Useful for when generating access code.
    $ zip [0..] fields

-- Get type of a field
fieldType :: AST.TypeName -> RooType
fieldType AST.BoolType = BoolType
fieldType AST.IntType = IntType
-- Parser grammar guarantees field type is integer or string, so we should
-- never get here.
fieldType _ = error "fieldType: Field is not integer or boolean"

-- Generate symbols for an array, checking for duplicate array type names
-- Also ensures size is >= 1.
stArray :: AST.ArrayDec -> SymTabMonad ()
stArray (AST.ArrayDec posn name rooType size) = do
    when (size < 1) (throwError $ ArrayTooSmall posn)
    typeAliases' <- gets typeAliases
    arrayType' <- arrayType posn rooType
    when (name `Map.member` typeAliases')
        (throwError $ DuplicateDefinition posn)
    modify (\s -> s {
        typeAliases = Map.insert name (arrayType' size) typeAliases' })

-- Construct type of array, checking for valid record type
-- Note: partially applied data constructor so size can be added by caller
arrayType :: Posn -> AST.TypeName -> SymTabMonad (Int -> RooType)
arrayType _ AST.BoolType = return $ ArrayType BoolType
arrayType _ AST.IntType = return $ ArrayType IntType
arrayType posn (AST.AliasType name) = do
    typeAliases' <- gets typeAliases
    case Map.lookup name typeAliases' of
        Just r@(RecordType _) -> return $ ArrayType r
        _ -> throwError $ BadArrayType posn

-- Construct symbols for procedure, checking for duplicate procedure names.
stProcedure :: AST.Proc -> SymTabMonad ()
stProcedure (AST.Proc posn name parameters locals _) = do
    procedures' <- gets procedures
    parameters' <- stParameters parameters
    locals' <- stLocals locals parameters'
    when (name `Map.member` procedures') (throwError $ DuplicateDefinition posn)
    let procedure = Procedure {
        -- Stack slots are allocated for parameters left to right,
        -- so their slot number is also their position in the procedure's call
        -- signature.
        procSig = sortBy (comparing varStackSlot) . Map.elems $ parameters',
        procSymTab = locals',
        procStackSize = stackSize locals' }
    modify (\s -> s {
        procedures = Map.insert name procedure procedures' })

-- Calculate number of stack slots needed to hold a variable
variableSize :: Variable -> Int
variableSize Variable {
    varType = ArrayType (RecordType fields) size,
    varByRef = False } = size * Map.size fields
variableSize Variable {
    varType = ArrayType _ size,
    varByRef = False } = size
variableSize Variable {
    varType = RecordType fields,
    varByRef = False } = Map.size fields
variableSize _ = 1

-- Calculate size of stack to hold a set of variables
stackSize :: (Map.Map String Variable) -> Int
stackSize = Map.foldr' ((+) . variableSize) 0

-- Generate variable entities arising as a result of a procedure's formal
-- arguments. Ensures there are no duplicate definitions.
stParameters :: [AST.Parameter] -> SymTabMonad (Map.Map String Variable)
stParameters parameters
  = foldM checkAndInsert' Map.empty $ parameters
      where
          checkAndInsert' m p@(AST.RefParam posn name rooType)
            = checkAndInsert True posn m name rooType
          checkAndInsert' m p@(AST.ValParam posn name rooType)
            = checkAndInsert False posn m name rooType
          checkAndInsert byRef posn m name rooType = do
              paramType' <- paramType posn rooType
              if name `Map.member` m
                 then throwError (DuplicateDefinition posn)
                 else return (Map.insert name (Variable {
                        varType = paramType',
                        varByRef = byRef,
                        varStackSlot = Map.size m }) m)

-- Get type of parameter, ensuring any array or record types are valid
paramType :: Posn -> AST.TypeName -> SymTabMonad (RooType)
paramType _ AST.BoolType = return $ BoolType
paramType _ AST.IntType = return $ IntType
paramType posn (AST.AliasType typeName) = do
    typeAliases' <- gets typeAliases
    let l = Map.lookup typeName typeAliases'
     in case l of
          Just t -> return t
          Nothing -> throwError $ BadVariableType posn

-- Convert AST list of procedure local variables to symtab map.
-- This is slightly complicated by AST.VarDec having a list of AST.Ident
-- that are all to have the same type. We need to expand each element into
-- a separate Variable.
stLocals :: [AST.VarDec] -> (Map.Map String Variable) -> SymTabMonad Locals
stLocals locals currentVariables
  = foldM checkAndInsert currentVariables $ expandVarDecs locals
      where
          expandVarDecs decs
            = [(v, AST.varPosn d, AST.varDecType d)
                    | d <- decs, 
                      v <- (AST.varDecNames d)]
          checkAndInsert m (name, posn, rooType) = do
              varType' <- paramType posn rooType
              if name `Map.member` m
                 then throwError (DuplicateDefinition posn)
                 else return (Map.insert name (Variable {
                        varType = varType',
                        varByRef = False,
                        varStackSlot = stackSize m }) m)
