module GreetIfCool2 where

    greetIfCool :: String -> IO()

    greetIfCool coolness =
        if cool coolness
            then putStrLn "eyyyy. What's shakin?"
        else
            putStrLn "pshhh."
        where cool v = v == "downright frosty yo"