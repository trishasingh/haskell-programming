module Print2 where

    main :: IO()
    main = do
        putStrLn "Count to four for me:"
        putStr "one "
        putStr "two "
        putStr "three, and "
        putStrLn "four!"