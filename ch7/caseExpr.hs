module CaseExpr where

    funcZ x = if x + 1 == 1 then "AWESOME" else "wut"

    funcZ' x =
        case x + 1 == 1 of
            True -> "AWESOME"
            False -> "wut"

    pal xs = 
        case xs == reverse xs of
            True -> "yes"
            False -> "no"

    pal' xs = 
        case y of
            True -> "yes"
            False -> "no"
        where y = xs == reverse xs
    

    