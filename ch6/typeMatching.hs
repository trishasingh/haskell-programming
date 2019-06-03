-- chapter exercises : match the types

module TypeMatching where

    i :: Num a => a
    i = 1
    -- Num is broadest typeclass

    f :: Fractional a => a
    f = 1.0
    -- Fractional

    f' :: RealFrac a => a
    f' = 1.0

    freud :: Ord a => a -> a
    freud x = x

    freud' :: Int -> Int
    freud' x = x

    myX = 1 :: Int
    sigmund :: Int -> Int
    sigmund x = myX

    -- import Data.List (sort)
    jung :: Ord a => [a] -> a -- or jung :: [Int] -> Int
    jung xs = head (sort xs)

    young :: Ord a => [a] -> a -- or young :: [String] -> String
    young xs = head (sort xs)

    mySort :: [Char] -> [Char]
    mySort = sort

    signifier :: [Char] -> Char
    signifier xs = head (mySort xs)
    




    
