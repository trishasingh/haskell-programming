module Ch6 where

    x = 10 :: Integer
    y = 5 :: Integer

    testType = (x+)
    -- This changes the type in the type in the type signature of (+) from Num to Integer

    addIntegers = (+) :: Integer -> Integer -> Integer
    -- Can't reverse this now to Num

    compareEx x y = compare x y

    maxOfThree x y z = max x (max y z)
