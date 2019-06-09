module SepString where

    sepString :: Char -> [Char] -> [[Char]]
    sepString pattern x
        | x == []   = []
        | otherwise = firstx : sepString pattern restx'
                          where firstx = takeWhile (/=pattern) x
                                restx  = dropWhile (/=pattern) x
                                restx' = if restx == [] 
                                         then [] 
                                         else tail restx

    firstSen = "Tyger Tyger, burning bright\n"
    secondSen = "In the forests of the night\n"
    thirdSen = "What immortal hand or eye\n"
    fourthSen = "Could frame thy fearful symmetry?"
    sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

    shouldEqual =
        [ "Tyger Tyger, burning bright"
        , "In the forests of the night"
        , "What immortal hand or eye"
        , "Could frame thy fearful symmetry?"
        ]

    main :: IO ()
    main =
        print $ "Are they equal? "
                ++ show (sepString '\n' sentences == shouldEqual)