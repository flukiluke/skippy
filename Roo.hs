-- Skippy, a compiler for the Roo language.
--
-- Submitted for assignment 1a of COMP90045, 2020
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

data Task
    = Parse | Pprint | Lex
    deriving (Eq, Show)

main :: IO ()
main
  = do
      progname <- getProgName
      args <- getArgs
      task <- checkArgs progname args
      case task of
        -- Dump AST with no particular format
        Parse
          -> do
              let [_, filename] = args
              input <- readFile filename
              let output = scan input parse
              case output of
                Right ast
                  -> putStrLn (show ast)
                Left err
                  -> do putStrLn err
                        exitWith (ExitFailure 2)
              -- putStrLn . show . (runAlex parse input)
        -- Pretty print the input program; format then suitable for input
        Pprint
          -> do
              let [_, filename] = args
              input <- readFile filename
              putStrLn "df"
              -- pprint . parse . alexScanTokens $ input
        -- Dump Lexer tokens. This option is not in the Roo spec, but
        -- convenient for debugging.
        Lex
          -> do
              let [_, filename] = args
              input <- readFile filename
              putStrLn "d" -- . show . alexScanTokens $ input

checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_]
  = do
      putStrLn ("Missing filename")
      exitWith (ExitFailure 1)
checkArgs _ ["-a", filename]
  = return Parse
checkArgs _ ["-p", filename]
  = return Pprint
checkArgs _ ["-l", filename]
  = return Lex
checkArgs progname _
  = do
      putStrLn ("Usage: " ++ progname ++ " -[pal] filename")
      exitWith (ExitFailure 1)

