module DoIO where 

    main :: IO Bool
    main = do
        c <- getChar
        c' <- getChar
        return (c == c')

    main' :: IO ()
    main' = do 
        c <- getChar 
        c' <- getChar 
        if c == c'
            then putStrLn "True"
            else return ()
    