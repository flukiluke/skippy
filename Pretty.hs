module Pretty where
import AST

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
    sequence $ fmap printProc procedures
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

printProc ps = do
    putStrLn "procedure"
    return ()
