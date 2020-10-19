-- Skippy, a compiler for the Roo language.
--
-- Submitted for assignment 1b of COMP90045, 2020
-- By Luke Ceddia [lceddia] and Ben Harper [bharper1]
-- 16 September 2020
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.
--
-- This is the main program. To compile this program you will need GHC, the
-- Glasgow Haskell Compiler. Then:
-- $ cabal install alex happy
-- $ make

module Main (main) where
import System.Environment (getProgName, getArgs)
import System.Exit (exitWith, ExitCode(..))
import Scanner (scan)
import Parser (parse)
-- import Pretty (pprint)
import SymbolTable (symtab)
import SemanticCheck (checkProgram)
-- import CodeGen (generateMachineCode)

main :: IO ()
main
  = do
      args <- getArgs
      input <- readFile . last $ args
      case compile input of
        Left e -> putStrLn e
        Right () -> putStrLn "OK"

compile :: String -> Either String ()
compile input = do
    ast <- scan input parse
    symbolTable <- symtab ast
    checkProgram symbolTable ast


