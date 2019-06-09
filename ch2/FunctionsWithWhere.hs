module FunctionsWithWhere where

    -- using where
    printInc n = print plusTwo
        where plusTwo = n + 2
    
    -- using let
    printInc2 n = 
        let plusTwo = n + 2
        in print plusTwo

    -- using lambda expr instead of let/in
    printInc2' n =
        (\plusTwo -> print plusTwo) (n + 2)

    -- intermission exercise 4
    addThree    = x + 3
        where x = 3
              y = 1000

    -- lambdas beneath let functions
    id' = \x -> x

    -- |let a = b in c| equivalent to |c where a = b| equivalent to |(\a -> c) b|
    -- |let x = 10 in x + 9001| equivalent to |(\x -> x + 9001) 10|

    -- more exercises: convert let to where
    -- let x = 3; y = 1000 in x * 3 + y
    -- test = x * 3 + y where x = 3; y = 1000