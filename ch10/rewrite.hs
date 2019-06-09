module Rewrite where

    import Data.Bool

    myOr :: [Bool] -> Bool
    myOr = foldr (||) False

    myAny :: (a -> Bool) -> [a] -> Bool 
    myAny f = foldr (\a b -> f a || b) False 

    myElem :: Eq a => a -> [a] -> Bool
    myElem a = foldr (\x y -> x == a || y) False 

    myElem' :: Eq a => a -> [a] -> Bool
    myElem' a = any (==a) 

    myReverse :: [a] -> [a]
    myReverse = foldl (flip (:)) []

    myMap :: (a -> b) -> [a] -> [b]
    myMap f = foldr ((:) . f) []

    myFilter :: (a -> Bool) -> [a] -> [a]
    myFilter f = 
        foldr (\a b -> bool b (a:b) (f a)) []

    squish :: [[a]] -> [a]
    squish = foldr (++) []

    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap f = foldr ((++) . f) []

    squishAgain :: [[a]] -> [a]
    squishAgain = squishMap id

    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy f (x:xs) = foldr (\a b -> bool b a (f a b == GT)) 
                           x xs

    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a 
    myMinimumBy f (x:xs) = foldr (\a b -> bool b a (f a b == LT))
                           x xs
