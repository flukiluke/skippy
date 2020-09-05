module Pretty where
import AST
import Data.List (intercalate, intersperse)

-- something?

pprint :: Program -> IO()

printRecord :: RecordDec -> IO()
printArray :: ArrayDec -> IO()
printProc :: Proc -> IO()
printStmt :: Int -> Stmt -> IO()
printExpr :: Expr -> String
printLval :: LValue -> String

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
    putStrLn $ whitespace indent ++ printLval lval ++ " <- " ++ printExpr expr ++ ";"

printStmt indent (Read lval) = do
    putStrLn $ whitespace indent ++ "write " ++ printLval lval ++ ";"

printStmt indent (Write expr) = do
    putStrLn $ whitespace indent ++ "write " ++ printExpr expr ++ ";"

printStmt indent (WriteLn expr) = do
    putStrLn $ whitespace indent ++ "writeln " ++ printExpr expr ++ ";"

printStmt indent (Call ident exprs) = do
    putStrLn $ whitespace indent ++ "call " ++ ident ++ "("
        ++ (intercalate ", " $ map printExpr exprs) ++ ");"

printStmt indent (If expr stmts1 stmts2) = do
    putStrLn $ whitespace indent ++ "if " ++ printExpr expr ++ " then"
    sequence $ fmap (printStmt $ indent + 1) stmts1
    if null stmts2
       then do putStrLn $ whitespace indent ++ "fi"
       else do
           putStrLn $ whitespace indent ++ "else"
           sequence $ fmap (printStmt $ indent + 1) stmts2
           putStrLn $ whitespace indent ++ "fi"

printStmt indent (While expr stmts) = do
    putStrLn $ whitespace indent ++ "while " ++ printExpr expr
    sequence $ fmap (printStmt $ indent + 1) stmts
    putStrLn $ whitespace indent ++ "od"

printLval (LId ident) = ident
printLval (LField id1 id2) = id1 ++ "." ++ id2
printLval (LArray ident expr) = ident ++ "[" ++ printExpr expr ++ "]"
printLval (LArrayField id1 expr id2) = id1 ++ "[" ++ printExpr expr ++ "]." ++ id2

printExpr (Lval lval) = printLval lval
printExpr (BoolLit bool) = show bool
printExpr (IntLit int) = show int
printExpr (StrLit str) = "\"" ++ str ++ "\""
printExpr (BinOpExpr op expr1 expr2) = printExpr expr1 ++ " " ++ show op ++ " " ++ printExpr expr2
printExpr (Lnot expr) = "not " ++ printExpr expr
printExpr (Negate expr) = "-" ++ printExpr expr
