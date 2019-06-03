module FilterExercises where

    multThree :: Integral a => [a] -> [a]
    multThree = filter (\x -> rem x 3 == 0)

    numMultThree :: Integral a => [a] -> Int
    numMultThree = length . filter (\x -> rem x 3 == 0)

    articles = ["a", "an", "the"]
    filterArticles :: String -> [String]
    filterArticles xs = filter (\x -> (not . elem x) articles) 
                        (words xs)