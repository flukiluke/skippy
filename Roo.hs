-- cabal install alex happy pretty-print

module Main (main) where
import System.Environment (getProgName, getArgs)
import System.Exit (exitWith, ExitCode(..))
import Text.Pretty.Simple as TextPretty (pPrint)
import Scanner (alexScanTokens)
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
        Parse
          -> do
              let [_, filename] = args
              input <- readFile filename
              TextPretty.pPrint . parse . alexScanTokens $ input
        Pprint
          -> do
              putStrLn "Pprint"
              {-
              let [_, filename] = args
               input <- readFile filename
               let output = ast input
               case output of
                 Right tree 
                   -> putStrLn (pp tree)
                 Left err 
                   -> do putStrLn "Parse error at "
                         print err
                         exitWith (ExitFailure 2) 
               -}
        Lex
          -> do
              let [_, filename] = args
              input <- readFile filename
              TextPretty.pPrint . alexScanTokens $ input

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

