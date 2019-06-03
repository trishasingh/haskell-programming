module DiffConcat where

    hello = "hello"
    world = "world"

    stringOut1 = hello ++ world
    stringOut2 = concat[hello, world]

    listOut1 = [hello] ++ [world]
    listOut2 = concat[[hello], [world]]

    listOfListOut1 = [hello ++ world]
    listOfListOut2 = [concat[hello, world]]
