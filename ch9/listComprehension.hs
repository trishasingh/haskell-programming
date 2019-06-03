module ListComprehension where
    
    import Data.List (intersperse)

    fOddSq :: Integer -> [Integer]
    fOddSq n = [x^2 | x <- [1..n], rem x 2 /= 0]

    multGen :: (Integral a, Integral b) => a -> b -> [a]
    multGen a b = [x^y | x <- [1..a], y <- [1, b], x^y < 200]
    -- rightmost list exhausted first

    genAcronym :: String -> String
    genAcronym xs = intersperse '.' 
                    [x | x <- xs, elem x ['A'..'Z']]
                    ++ "."

    -- exercises
    mySqr = [x^2 | x <- [1..5]]
    myCube = [y^3 | y <- [1..5]]

    sqCubeTuple = [(x, y) | x <- mySqr, y <- myCube]
    sqCubeTuple50 = [(x, y) | x <- mySqr, y <- myCube,
                              x < 50, y < 50]
    tupleLen = length sqCubeTuple50

    -- vs filtering
    input = "abracadabra"
    ex1 = filter (\x -> elem x "aeiou")
    ex2 t = [x | x <- t, elem x "aeiou"]



