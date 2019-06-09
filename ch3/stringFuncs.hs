module StringFuncs where

    dropLast x = take (length x - 1) x

    nthLetter x n = x !! n

    lastWord x = drop (length x - 8) x

    test = "Curry is awesome!"

