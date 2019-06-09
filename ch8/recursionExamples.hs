module RecursionExamples where

    factorial :: Integer -> Integer
    factorial 0 = 1
    factorial n = n * factorial (n - 1)

    -- increase by 1 specified number of times
    incTimes :: (Eq a, Num a) => a -> a -> a
    incTimes 0 n = n
    incTimes times n = 1 + incTimes (times - 1) n

    -- apply a function specified number of times
    applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
    applyTimes 0 f b = b
    applyTimes n f b = f . applyTimes (n - 1) f $ b

    -- incTimes redefined
    incTimes' :: (Eq a, Num a) => a -> a -> a
    incTimes' times n = applyTimes times (+1) n

    -- Fibonacci
    fibonacci :: Integral a => a -> a 
    fibonacci 0 = 0
    fibonacci 1 = 1
    fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)