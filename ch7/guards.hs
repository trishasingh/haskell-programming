module Guards where

    myAbs :: Integer -> Integer
    myAbs x
        | x < 0     = (-x)
        | otherwise = x

    -- look at blood sodium levels
    bloodNa :: Integer -> String
    bloodNa x
        | x < 135   = "Too low"
        | x > 145   = "Too high"
        | otherwise = "in range"

    -- check if 3 sides form a right triangle
    -- last side is hypotenuse
    isRight :: (Num a, Eq a) => a -> a -> a -> String
    isRight a b c
        | a^2 + b^2 == c^2 = "RIGHT ON"
        | otherwise        = "Nope"

    -- calculates dog's age in human yrs
    dogYrs :: Integer -> Integer
    dogYrs x
        | x <= 0    = 0
        | x <= 1    = x * 15
        | x <= 2    = x * 12
        | x <= 4    = x * 8
        | otherwise = x * 6

    -- input two grades and get the average percentage
    avgGrade :: (Fractional a, Ord a) => a -> a -> String
    avgGrade x y
        | p >= 0.9  = "A"
        | p >= 0.8  = "B"
        | p >= 0.7  = "C"
        | p >= 0.59 = "D"
        | p >= 0    = "F"
        | otherwise = "Enter valid grade"
        where p = (x + y)/200


    