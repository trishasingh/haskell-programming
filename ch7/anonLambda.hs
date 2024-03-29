module AnonLambda where

    addOne = \x -> x + 1

    addOneIfOdd n = case odd n of 
        True -> f n
        False -> n
        where f = \x -> x + 1

    addFive = \x y -> if (x > y) then y else x + 5

    mFlip f x y = f y x