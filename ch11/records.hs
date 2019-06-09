module Records where 

    data Person = 
        Person { name :: String
               , age :: Int }
               deriving (Eq, Show)

    jm = Person "julie" 108
    ca = Person "chris" 16

    -- :t name -- name is a function
    -- name jm
    