module AvgChars where

    seekritFunc x =
        div (sum (map length (words x)))
        (length (words x))

    avgChars :: String -> Double
    avgChars x =
        fromIntegral nChars / fromIntegral nWords
            where nChars = sum $ map length (words x)
                  nWords = length . words $ x