module ChapterExercises where

    -- returns ten's digit of integral arg
    tensDigit :: Integral a => a -> a
    tensDigit x = d
        where (xLast, _) = x `divMod` 10
              (_, d)     = xLast `divMod` 10

    -- returns hundered's digit
    hunsDigit :: Integral a => a -> a
    hunsDigit x = d 
        where (xLast, _) = x `divMod` 100
              (_, d)     = xLast `divMod` 10

    hunsDAlt :: Integral a => a -> a
    hunsDAlt = flip mod 10 . flip div 100

    -- foldBool implemented 3 ways
    foldBoolPattern :: a -> a -> Bool -> a 
    foldBoolPattern x y True = x 
    foldBoolPattern x y False = y

    foldBoolCase :: a -> a -> Bool -> a 
    foldBoolCase x y b = 
        case b of
            True -> x 
            False -> y

    foldBoolGuard :: a -> a -> Bool -> a 
    foldBoolGuard x y b
            | b         = x 
            | otherwise = y


    g :: (a -> b) -> (a, c) -> (b, c)
    g f (x, y) = (f x, y)

