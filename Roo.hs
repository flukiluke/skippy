-- Skippy, a compiler for the Roo language.
--
-- Submitted for assignment 3 of COMP90045, 2020
-- By Luke Ceddia [lceddia] and Ben Harper [bharper1]
-- 28 October 2020
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.
--
-- This is the main program. To compile this program you will need GHC, the
-- Glasgow Haskell Compiler. Then:
-- $ cabal install alex happy
-- $ make

module Main (main) where
import System.IO (hPutStrLn, stderr)
import System.Environment (getProgName, getArgs)
import System.Exit (exitWith, ExitCode(..))
import Scanner (scan)
import Parser (parse)
import SymbolTable (makeSymtab)
import SemanticCheck (checkProgram)
import ErrorHandling (semanticError, SemanticError)
import CodeGen (generateMachineCode)

main :: IO ()
main
  = do
      args <- getArgs
      input <- readFile . last $ args
      case compile input of
        Left e -> hPutStrLn stderr e >> exitWith (ExitFailure 1)
        Right code -> mapM_ putStrLn code

-- A semantic stage is one that can produce a SemanticError
semanticStage :: String -> Either SemanticError a -> Either String a
semanticStage input result
  = case result of
      Left e -> Left $ semanticError e input
      Right s -> Right s

compile :: String -> Either String [String]
compile input = do
    ast <- scan input parse
    symbolTable <- semanticStage input $ makeSymtab ast
    semanticStage input $ checkProgram symbolTable ast
    return $ map show $ generateMachineCode symbolTable ast
