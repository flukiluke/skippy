module Pretty where
import AST
import Data.List (intercalate, intersperse)

-- something?

pprint :: Program -> IO()

printRecord :: RecordDec -> IO()
printArray :: ArrayDec -> IO()
printProc :: Proc -> IO()

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
    putStrLn $ "    { " ++ (show t1) ++ " " ++ id1
    sequence $ fmap (\(FieldDec i t)
        -> putStrLn $ "    ; " ++ (show t) ++ " " ++ i) fs
    putStrLn $ "    } " ++ ident ++ ";"

printArray (ArrayDec ident typename size)  = do
    putStrLn $ "array[" ++ show size ++ "] " ++ show typename
        ++ " " ++ show ident ++ ";"
    return ()

printProc (Proc ident parameters vardecs stmts) = do
    -- print header
    putStrLn $ "procedure " ++ ident ++ " ("
        ++ (intercalate ", " $ map show parameters) ++ ")"
    sequence $ fmap (\x -> putStrLn $ "    " ++ show x ++ ";") vardecs
    putStrLn "{"
    putStrLn "}"
    return ()
