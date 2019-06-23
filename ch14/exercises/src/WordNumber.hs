module WordNumber 
       ( digitToWord
       , digits
       , wordNumber )
       where

    import Data.List (intercalate)

    digitToWord :: Int -> String
    digitToWord n
        | n == 0 = "zero"
        | n == 1 = "one"
        | n == 2 = "two"
        | n == 3 = "three"
        | n == 4 = "four"
        | n == 5 = "five"
        | n == 6 = "six"
        | n == 7 = "seven"
        | n == 8 = "eight"
        | n == 9 = "nine"
        | otherwise = "not a digit"

    digits :: Int -> [Int]
    digits n
        | n == 0    = []
        | otherwise = digits (div n 10) ++ [rem n 10]
    
    wordNumber :: Int -> String
    wordNumber n = intercalate "-" $ map digitToWord $ digits n


    -- 200
    -- 345
    -- 5
    -- 450