printTwice :: String -> IO ()
printTwice str = putStrLn str >> putStrLn str