module CharExercises where

    import Data.Char

    remUpper :: String -> String
    remUpper xs = [x | x <- xs, isUpper x]

    capFirst :: String -> String
    capFirst (x:xs) = toUpper x : xs

    capAll :: String -> String
    capAll [] = []
    capAll (x:xs) = toUpper x :
                    capAll xs

    firstCap :: String -> Char
    firstCap = toUpper . head 