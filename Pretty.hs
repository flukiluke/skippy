module Pretty where
import AST
import Data.List (intercalate, intersperse)

-- something?

pprint :: Program -> IO()

printRecord :: RecordDec -> IO()
printArray :: ArrayDec -> IO()
printProc :: Proc -> IO()
printStmt :: Int -> Stmt -> IO()

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
    putStrLn $ whitespace indent ++ show lval ++ " <- " ++ show expr ++ ";"

printStmt indent (Read lval) = do
    putStrLn $ whitespace indent ++ "write " ++ show lval ++ ";"

printStmt indent (Write expr) = do
    putStrLn $ whitespace indent ++ "write " ++ show expr ++ ";"

printStmt indent (WriteLn expr) = do
    putStrLn $ whitespace indent ++ "writeln " ++ show expr ++ ";"

printStmt indent (Call ident exprs) = do
    putStrLn $ whitespace indent ++ "call " ++ ident ++ " ("
        ++ (intercalate ", " $ map show exprs) ++ ");"

printStmt indent (If expr stmts1 stmts2) = do
    putStrLn $ whitespace indent ++ "if " ++ show expr ++ " then"
    sequence $ fmap (printStmt $ indent + 1) stmts1
    if null stmts2
       then do putStrLn $ whitespace indent ++ "fi"
       else do
           putStrLn $ whitespace indent ++ "else"
           sequence $ fmap (printStmt $ indent + 1) stmts2
           putStrLn $ whitespace indent ++ "fi"

printStmt indent (While expr stmts) = do
    putStrLn $ whitespace indent ++ "while " ++ show expr
    sequence $ fmap (printStmt $ indent + 1) stmts
    putStrLn $ whitespace indent ++ "od"
