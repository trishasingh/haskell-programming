module StandardFuncs where

    import Data.Bool

    myAnd :: [Bool] -> Bool
    myAnd [] = True
    myAnd (x:xs) = x && myAnd xs

    myOr :: [Bool] -> Bool
    myOr [] = False
    myOr (x:xs) = x || myOr xs 

    myAny :: (a -> Bool) -> [a] -> Bool 
    myAny f xs = myOr . map f $ xs 

    myElem :: Eq a => a -> [a] -> Bool
    myElem a [] = False
    myElem a (x:xs) = a == x || myElem a xs

    myElem' :: Eq a => a -> [a] -> Bool 
    myElem' a xs = any (==a) xs

    myReverse :: [a] -> [a]
    myReverse [] = []
    myReverse (x:xs) = myReverse xs ++ [x]

    squish :: [[a]] -> [a]
    squish [] = []
    squish (x:xs) = x ++ squish xs

    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap f [] = []
    squishMap f (x:xs) = f x ++ squishMap f xs

    squishAgain :: [[a]] -> [a]
    squishAgain xs = squishMap id xs

    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy f (x1:x2:xs)
        | length xs == 0 = xMax
        | otherwise = myMaximumBy f (xMax:xs)
            where xMax = bool x2 x1 (f x1 x2 == GT)

    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
    myMinimumBy f (x1:x2:xs)
        | length xs == 0 = xMin
        | otherwise = myMinimumBy f (xMin:xs)
            where xMin = bool x2 x1 (f x1 x2 == LT)

    myMaximum :: Ord a => [a] -> a 
    myMaximum xs = myMaximumBy compare xs

    myMinimum :: Ord a => [a] -> a 
    myMinimum xs = myMinimumBy compare xs

    