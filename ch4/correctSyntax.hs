module CorrectSyntax where

    x = (+)
    f1 xs = x w 1 
        where w = length xs

    -- identity
    f2 = \x -> x

    f3 = \xs -> head xs

    f4 x = fst x

    f5 (x:xs) = x