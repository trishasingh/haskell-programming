module Print3 where

    myGreeting :: String
    myGreeting = "hello" ++ " world"

    secondGreeting :: String
    secondGreeting = concat ["hello", " ", "world"]

    main :: IO()
    main = do
        putStrLn myGreeting
        putStrLn secondGreeting