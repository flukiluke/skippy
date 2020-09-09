-- Skippy, a compiler for the Roo language.
--
-- Submitted for assignment 1a of COMP90045, 2020
-- By Luke Ceddia [lceddia] and Ben Harper [bharper1]
-- 16 September 2020
--
-- This program is licensed under the MIT license; see the LICENCE file for
-- full details.
--
-- This file exports the pprint function, which pretty prints a Roo program.
module Pretty (pprint) where
import AST
import Data.List (intercalate, intersperse)

-- Various functions for printing parts of a program. They operate by composing
-- IO() monads; formatting rules are taken directly from the assignment spec.

pprint :: Program -> IO()
pprint (Program records arrays procedures) = do
    sequence $ fmap printRecord records
    sequence $ fmap printArray arrays
    if null records && null arrays
       then do return ()
       else do putStrLn ""
    sequence $ intersperse (putStrLn "") $ fmap printProc procedures
    return ()

printRecord :: RecordDec -> IO()
printRecord (RecordDec ident ((FieldDec id1 t1):fs)) = do
    putStrLn "record"
    putStrLn $ "    { " ++ show t1 ++ " " ++ id1
    sequence $ fmap (\f -> putStrLn $ "    ; " ++ show f) fs
    putStrLn $ "    } " ++ ident ++ ";"

printArray :: ArrayDec -> IO()
printArray (ArrayDec ident typename size)  = do
    putStrLn $ "array[" ++ show size ++ "] " ++ show typename
        ++ " " ++ ident ++ ";"
    return ()

printProc :: Proc -> IO()
printProc (Proc ident parameters vardecs stmts) = do
    -- print header
    putStrLn $ "procedure " ++ ident ++ " ("
        ++ (intercalate ", " $ map show parameters) ++ ")"
    sequence $ fmap (\x -> putStrLn $ "    " ++ show x ++ ";") vardecs
    putStrLn "{"
    sequence $ fmap (printStmt 1) stmts
    putStrLn "}"

printStmt :: Int -> Stmt -> IO()
printStmt indent (Assign lval expr) = do
    putStr $ whitespace indent
    printLval lval
    putStr " <- "
    printExpr expr
    putStrLn ";"

printStmt indent (Read lval) = do
    putStr $ whitespace indent ++ "read "
    printLval lval
    putStrLn ";"

printStmt indent (Write expr) = do
    putStr $ whitespace indent ++ "write "
    printExpr expr
    putStrLn ";"

printStmt indent (WriteLn expr) = do
    putStr $ whitespace indent ++ "writeln "
    printExpr expr
    putStrLn ";"

printStmt indent (Call ident exprs) = do
    putStr $ whitespace indent ++ "call " ++ ident ++ "("
    sequence $ intersperse (putStr ", ") $ fmap printExpr exprs
    putStrLn ");"

printStmt indent (If expr stmts1 stmts2) = do
    putStr $ whitespace indent ++ "if "
    printExpr expr
    putStrLn " then"
    sequence $ fmap (printStmt $ indent + 1) stmts1
    if null stmts2
       then do putStrLn $ whitespace indent ++ "fi"
       else do
           putStrLn $ whitespace indent ++ "else"
           sequence $ fmap (printStmt $ indent + 1) stmts2
           putStrLn $ whitespace indent ++ "fi"

printStmt indent (While expr stmts) = do
    putStr $ whitespace indent ++ "while "
    printExpr expr
    putStrLn " do"
    sequence $ fmap (printStmt $ indent + 1) stmts
    putStrLn $ whitespace indent ++ "od"

printLval :: LValue -> IO()
printLval (LId ident) = putStr ident
printLval (LField id1 id2) = putStr $ id1 ++ "." ++ id2
printLval (LArray ident expr) = do
    putStr $ ident ++ "["
    printExpr expr
    putStr "]"
printLval (LArrayField id1 expr id2) = do
    putStr $ id1 ++ "["
    printExpr expr 
    putStr $ "]." ++ id2

printExpr :: Expr -> IO()
-- We can't use (show str) because that doesn't handle unicode characters,
-- as per the accounting.roo example program.
printExpr (StrLit str) = putStr ('"' : (rooEscape str) ++ "\"")
printExpr (Lval lval) = printLval lval
printExpr (BoolLit True) = putStr "true"
printExpr (BoolLit False) = putStr "false"
printExpr (IntLit int) = putStr (show int)
-- Insert parentheses whereever the natural precedence differs from the
-- actual parse tree.
printExpr e@(BinOpExpr op expr1 expr2) = do
    if precedence e > precedence expr1
       then do
           putStr "("
           printExpr expr1
           putStr ")"
       else printExpr expr1
    putStr $ " " ++ show op ++ " "
    if precedence e >= precedence expr2
       then do
           putStr "("
           printExpr expr2
           putStr ")"
       else printExpr expr2
printExpr e@(PreOpExpr op expr) = do
    putStr $ show op
    if precedence e > precedence expr
       then do
           putStr "("
           printExpr expr
           putStr ")"
       else printExpr expr

--
-- Helper functions
--

-- Converts special characters back to backslash escapes
rooEscape :: String -> String
rooEscape ('\n':xs) = '\\':'n':(rooEscape xs)
rooEscape ('\t':xs) = '\\':'t':(rooEscape xs)
rooEscape ('\"':xs) = '\\':'"':(rooEscape xs)
rooEscape (x:xs) = x:(rooEscape xs)
rooEscape x = x

-- Generate sufficient whitespace for i levels of indent
whitespace :: Int -> String
whitespace i = concat $ take i $ repeat "    "

-- Provides precedence values of various operators, equivalent to the
-- precedence declarations in the Happy parser. Used for adding parentheses
-- in the right spots.
-- Note that 8 is considered highest precedence, and a default for anything
-- we don't recognise.
precedence :: Expr -> Int
precedence (BinOpExpr o _ _) = maybe 8 id $ lookup o [
    (Op_or, 1), (Op_and, 2), (Op_eq, 4), (Op_neq, 4), (Op_lt, 4),
    (Op_lteq, 4), (Op_gt, 4), (Op_gteq, 4), (Op_plus, 5), (Op_minus, 5),
    (Op_mult, 6), (Op_divide, 6)]
precedence (PreOpExpr o _)
    = maybe 8 id $ lookup o [(Op_not, 3), (Op_negate, 8)]
precedence _ = 8
