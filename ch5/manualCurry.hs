module ManualCurry where

    nonsense :: Bool -> Integer
    nonsense True = 805
    nonsense False = 31337

    typicalCurriedFunction :: Integer -> Bool -> Integer
    typicalCurriedFunction i b = i + (nonsense b)

    -- this lambda format is called anonymous function
    anonymous :: Integer -> Bool -> Integer
    anonymous = \i b -> i + (nonsense b)

    anonymousAndManuallyNested :: Integer -> Bool -> Integer
    anonymousAndManuallyNested = \i -> \b -> (i + nonsense b)