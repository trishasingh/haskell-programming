module GreetIfCool3 where

    greetIfCool :: String -> IO ()
    greetIfCool x =
        case cool of
            True -> putStrLn "eyyy. what's shakin'"
            False -> putStrLn "pshh"
        where cool = x == "sup"