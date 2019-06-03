module Arith3Broken where

    main1 :: IO ()

    main1 = let blah = negate 1 in do
        print $ 1 + 2
        putStrLn $ "10"
        print $ negate (-1)
        print $ (+) 1 blah

    -- define blah within print expression
    main2 :: IO ()
    main2 = do
        print $ 1 + 2
        putStrLn $ "10"
        print $ negate (-1)
        print $ (+) 1 blah
            where blah = negate 1

        
