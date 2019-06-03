-- here, cool is a function rather than a value defined against the argument

module GreetIfCool2 where

    greetIfCool :: String -> IO()

    greetIfCool coolness =
        if cool coolness
            then putStrLn "eyyy. What's shakin'"
        else
            putStrLn "pshhh"
        where cool v = v == "downright frosty yo"