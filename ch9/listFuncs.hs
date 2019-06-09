module ListFuncs where

    myTail :: [a] -> [a]
    myTail []       = []
    myTail (_ : xs) = xs 

    safeTail :: [a] -> Maybe [a]
    safeTail []       = Nothing
    safeTail (x : []) = Nothing
    safeTail (_ : xs) = Just xs

    myEnumFromTo :: (Ord a, Enum a) => a -> a -> [a]
    myEnumFromTo x y 
        | x > y = []
        | otherwise =  x : myEnumFromTo (succ x) y

    map' :: (a -> b) -> [a] -> [b]
    map' _ [] = []
    map' f (x : xs) = f x : map' f xs 

    anonExample1 :: Integral a => [a] -> [a]
    anonExample1 = map (\x -> if odd x then -x else x)

    anonExample2 :: Integral a => [a] -> [a]
    anonExample2 = filter (\x -> rem x 2 == 0)

    filter' :: (a -> Bool) -> [a] -> [a]
    filter' _ [] = []
    filter' predicate (x : xs) 
        | predicate x = x : filter predicate xs 
        | otherwise   = filter predicate xs

    -- zip functions
    zip' :: [a] -> [b] -> [(a, b)]
    zip' as bs 
        | length as == 0 || length bs == 0 = []
        | otherwise = (head as, head bs) : 
                      zip' (tail as) (tail bs)

    zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith' f as bs
        | length as == 0 || length bs == 0 = []
        | otherwise = (f (head as) (head bs)) :
                      zipWith' f (tail as) (tail bs)

    zipRe :: [a] -> [b] -> [(a, b)]
    zipRe as bs = zipWith' (,) as bs


