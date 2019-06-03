module Composition where

    -- take the first n natural numbers
    f :: Int -> [Integer] 
    f x = take x . enumFrom $ 1

    -- sum of first n odd numbers
    -- write in pointfree style
    sumOdd :: Int -> Integer
    sumOdd = sum . flip take (filter odd . enumFrom $ 1)

    -- count a's in a string
    countA :: String -> Int
    countA = length . filter (=='a')


