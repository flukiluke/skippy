module Pretty where
import AST
import Data.List (intercalate, intersperse)

pprint :: Program -> IO()

printRecord :: RecordDec -> IO()
printArray :: ArrayDec -> IO()
printProc :: Proc -> IO()
printStmt :: Int -> Stmt -> IO()
printExpr :: Expr -> IO()
printLval :: LValue -> IO()

whitespace :: Int -> String

whitespace i = concat $ take i $ repeat "    "

pprint (Program records arrays procedures) = do
    sequence $ fmap printRecord records
    sequence $ fmap printArray arrays
    if null records && null arrays
       then do return ()
       else do putStrLn ""
    sequence $ intersperse (putStrLn "") $ fmap printProc procedures
    return ()

printRecord (RecordDec ident ((FieldDec id1 t1):fs)) = do
    putStrLn "record"
    putStrLn $ "    { " ++ show t1 ++ " " ++ id1
    sequence $ fmap (\f -> putStrLn $ "    ; " ++ show f) fs
    putStrLn $ "    } " ++ ident ++ ";"

printArray (ArrayDec ident typename size)  = do
    putStrLn $ "array[" ++ show size ++ "] " ++ show typename
        ++ " " ++ ident ++ ";"
    return ()

printProc (Proc ident parameters vardecs stmts) = do
    -- print header
    putStrLn $ "procedure " ++ ident ++ " ("
        ++ (intercalate ", " $ map show parameters) ++ ")"
    sequence $ fmap (\x -> putStrLn $ "    " ++ show x ++ ";") vardecs
    putStrLn "{"
    sequence $ fmap (printStmt 1) stmts
    putStrLn "}"

printStmt indent (Assign lval expr) = do
    putStr $ whitespace indent
    printLval lval
    putStr " <- "
    printExpr expr
    putStrLn ";"

printStmt indent (Read lval) = do
    putStr $ whitespace indent ++ "write "
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
    putStrLn ""
    sequence $ fmap (printStmt $ indent + 1) stmts
    putStrLn $ whitespace indent ++ "od"

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

printExpr (Lval lval) = printLval lval
printExpr (BoolLit bool) = putStr (show bool)
printExpr (IntLit int) = putStr (show int)
printExpr (StrLit str) = putStr (show str)
printExpr (BinOpExpr op expr1 expr2) = do
    printExpr expr1
    putStr $ " " ++ show op ++ " "
    printExpr expr2
printExpr (Lnot expr) = do
    putStr "not "
    printExpr expr
printExpr (Negate expr) = do
    putStr "-"
    printExpr expr
