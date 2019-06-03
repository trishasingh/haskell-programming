module TypeInference where
    
    myGreetString x = x ++ "Julie"
    -- :t myGreetString

    myGreetPoly x y = x ++ y
    -- :t myGreetPoly

    typeInference1 :: Num a => a -> a -> a
    typeInference1 x y = x + y + 3
    -- :t typeInference1

    typeInference2 x y = x + y + 3
    -- :t typeInference2
    -- We see that Haskell compiler automatically assigns most polymorphic type
    -- same output as typeInference1