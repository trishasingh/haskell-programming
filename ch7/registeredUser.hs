module RegisteredUser where

    newtype Username = Username String
    newtype AccountNumber = AccountNumber Integer

    data User = UnregisteredUser 
              | RegisteredUser Username AccountNumber

    printUser :: User -> IO ()
    printUser UnregisteredUser = putStrLn "UnregisteredUser"
    printUser (RegisteredUser (Username name)
                              (AccountNumber acctNum))
              = putStrLn $ name ++ " " ++ show acctNum

-- printUser $ RegisteredUser (Username "Trisha") (AccountNumber 45)
-- myUser = (Username "callen")
-- myAcct = myAcct = (AccountNumber 10456)
-- printUser $ RegisteredUser myUser myAcct