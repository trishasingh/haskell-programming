module RecursionExercises where

    sumNum :: (Eq a, Num a) => a -> a
    sumNum 0 = 0
    sumNum n = n + sumNum (n - 1)

    multUsingSum :: Integral a => a -> a -> a
    multUsingSum x y
        | y == 0    = 0
        | otherwise = x + multUsingSum x (y - 1)

    mc91 :: Integral a => a -> a
    mc91 n 
        | n > 100   = n - 10
        | otherwise = mc91 . mc91 $ n + 11