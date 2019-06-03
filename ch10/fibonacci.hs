module Fibonacci where

    fibs = 1 : scanl (+) 1 fibs
    fibsN = (!!) fibs

    -- Exercises
    fibs20 = take 20 $ fibs

    fibs100 = takeWhile (<100) fibs

    factorial = (!!) $ scanl (*) 1 [1..]
