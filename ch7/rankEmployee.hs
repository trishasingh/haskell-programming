module RankEmployee where

    data Employee = Coder
                  | Manager
                  | Veep
                  | CEO 
                  deriving (Eq, Ord, Show)

    report :: Employee -> Employee -> IO ()
    report e e' = 
        putStrLn $ show e ++ " is the boss of " ++ show e'
        
    employeeRank :: (Employee -> Employee -> Ordering) 
                  -> Employee 
                  -> Employee 
                  -> IO ()
    employeeRank f e e' =
        case f e e' of
            GT -> report e e'
            LT -> (flip report) e e'
            EQ -> putStrLn "Neither employee is the boss"

    -- Argument function: this function ranks coders above all
    compare' :: Employee -> Employee -> Ordering
    compare' Coder Coder = EQ
    compare' Coder _     = GT
    compare' _ Coder     = LT
    compare' e e'        = compare e e'


