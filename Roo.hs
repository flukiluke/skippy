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
import Pretty (pprint)
import StaticVerify
import CodeGen (generateMachineCode)

data Task
    = Parse | Pprint | Lex | Compile
    deriving (Eq, Show)

main :: IO ()
main
  = do
      progname <- getProgName
      args <- getArgs
      task <- checkArgs progname args
      let filename = last args
      input <- readFile filename
      let output = scan input parse
      case task of
        -- Dump AST with no particular format
        Parse
          -> case output of
                Right ast
                  -> putStrLn (show ast)
                Left err
                  -> putStrLn err >> exitWith (ExitFailure 2)
        -- Pretty print the input program; format then suitable for input
        Pprint
          -> case output of
                Right ast
                  -> pprint ast
                Left err
                  -> putStrLn err >> exitWith (ExitFailure 2)
        Compile
          -> case output of
               Right ast
                  -> putStrLn . show . symtab $ ast
               Left err
                  -> putStrLn err >> exitWith (ExitFailure 2)

checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_]
  = do
      putStrLn ("Missing filename")
      exitWith (ExitFailure 1)
checkArgs _ ["-a", filename]
  = return Parse
checkArgs _ ["-p", filename]
  = return Pprint
checkArgs _ [filename]
  = return Compile
checkArgs progname _
  = do
      putStrLn ("Usage: " ++ progname ++ " -[pa] filename")
      exitWith (ExitFailure 1)
