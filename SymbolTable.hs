-- Skippy, a compiler for the Roo language
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
import SemanticErrors

type ProcSig = [RooType]

type Locals = Map.Map String Variable

data SymbolTable = SymbolTable {
                    typeAliases :: Map.Map String RooType,
                    procedures :: Map.Map String Procedure }
                    deriving (Eq, Show)

data Procedure = Procedure {
                    procSig :: [Variable],
                    procSymTab :: Locals,
                    procStackSize :: Int }
                    deriving (Eq, Show)

data RooType
    = IntType
    | BoolType
    | StringType
    | ArrayType RooType Int
    -- Record elements store their type and position in the record
    | RecordType (Map.Map String (Int, RooType))
    deriving (Eq, Show)

data Variable = Variable {
                    varType :: RooType,
                    varByRef :: Bool,
                    varStackSlot :: Int }
                    deriving (Eq, Show)

type SymTabMonad = ExceptT SemanticError (State SymbolTable)

emptySymTab :: SymbolTable
emptySymTab = SymbolTable Map.empty Map.empty

symtab :: AST.Program -> Either String SymbolTable
symtab program = case runState (runExceptT (stProgram program)) emptySymTab of
                   (Left e, _) -> Left . show $ e
                   (Right _, s) -> Right s

getProc :: SymbolTable -> String -> Procedure
getProc symbolTable procName
  = (procedures symbolTable) Map.! procName

getLocal :: Locals -> String -> Variable
getLocal locals name
  = locals Map.! name

stProgram :: AST.Program -> SymTabMonad ()
stProgram (AST.Program recordDecs arrayDecs procs) = do
    mapM stRecord recordDecs
    mapM stArray arrayDecs
    mapM_ stProcedure procs

stRecord :: AST.RecordDec -> SymTabMonad ()
stRecord (AST.RecordDec name fieldDecs) = do
    currentSymTab <- get
    types <- gets typeAliases
    fields <- stFields fieldDecs
    when (name `Map.member` types) (throwError $ DuplicateDefinition 0 0)
    modify (\s -> s { typeAliases = Map.insert name (RecordType fields) types })

stFields :: [AST.FieldDec] -> SymTabMonad (Map.Map String (Int, RooType))
stFields fields
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

stArray :: AST.ArrayDec -> SymTabMonad ()
stArray (AST.ArrayDec name rooType size) = do
    when (size < 1) (throwError $ ArrayTooSmall 0 0)
    typeAliases' <- gets typeAliases
    arrayType' <- arrayType rooType
    when (name `Map.member` typeAliases') (throwError $ DuplicateDefinition 0 0)
    modify (\s -> s {
        typeAliases = Map.insert name (arrayType' size) typeAliases' })

-- Note: partially applied data constructor so size can be added by caller
arrayType :: AST.TypeName -> SymTabMonad (Int -> RooType)
arrayType AST.BoolType = return $ ArrayType BoolType
arrayType AST.IntType = return $ ArrayType IntType
arrayType (AST.AliasType name) = do
    typeAliases' <- gets typeAliases
    case Map.lookup name typeAliases' of
        Just r@(RecordType _) -> return $ ArrayType r
        _ -> throwError $ BadArrayType 0 0

stProcedure :: AST.Proc -> SymTabMonad ()
stProcedure (AST.Proc name parameters locals _) = do
    procedures' <- gets procedures
    parameters' <- stParameters parameters
    locals' <- stLocals locals parameters'
    when (name `Map.member` procedures') (throwError $ DuplicateDefinition 0 0)
    let procedure = Procedure {
        procSig = sortBy (comparing varStackSlot) . Map.elems $ parameters',
        procSymTab = locals',
        procStackSize = stackSize locals' }
    modify (\s -> s {
        procedures = Map.insert name procedure procedures' })

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

stackSize :: (Map.Map String Variable) -> Int
stackSize = Map.foldr' ((+) . variableSize) 0

stParameters :: [AST.Parameter] -> SymTabMonad (Map.Map String Variable)
stParameters parameters
  = foldM checkAndInsert' Map.empty $ parameters
      where
          checkAndInsert' m p@(AST.RefParam name rooType)
            = checkAndInsert True m name rooType
          checkAndInsert' m p@(AST.ValParam name rooType)
            = checkAndInsert False m name rooType
          checkAndInsert byRef m name rooType = do
              paramType' <- paramType rooType
              if name `Map.member` m
                 then throwError (DuplicateDefinition 0 0)
                 else return (Map.insert name (Variable {
                        varType = paramType',
                        varByRef = byRef,
                        varStackSlot = Map.size m }) m)

paramType :: AST.TypeName -> SymTabMonad (RooType)
paramType AST.BoolType = return $ BoolType
paramType AST.IntType = return $ IntType
paramType (AST.AliasType typeName) = do
    typeAliases' <- gets typeAliases
    let l = Map.lookup typeName typeAliases'
     in case l of
          Just t -> return t
          Nothing -> throwError $ BadVariableType 0 0

stLocals :: [AST.VarDec] -> (Map.Map String Variable) -> SymTabMonad Locals
stLocals locals currentVariables
  = foldM checkAndInsert currentVariables $ expandVarDecs locals
      where
          expandVarDecs decs
            = [(v, AST.varDecType d) | d <- decs, v <- (AST.varDecNames d)]
          checkAndInsert m (name, rooType) = do
              varType' <- paramType rooType
              if name `Map.member` m
                 then throwError (DuplicateDefinition 0 0)
                 else return (Map.insert name (Variable {
                        varType = varType',
                        varByRef = False,
                        varStackSlot = stackSize m }) m)
