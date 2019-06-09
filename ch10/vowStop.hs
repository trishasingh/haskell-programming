module VowStop where

    stops = "pbtdkg"
    vowels = "aeiou"

    f :: [a] -> [a] -> [(a, a, a)]
    f xs ys = [(x,y,z) | x <- xs, y <- ys, z <- xs]

    f' :: String 
       -> String 
       -> [(Char, Char, Char)]
    f' xs ys = [(x,y,z) | x <- xs, y <- ys, z <- xs,
                          x == 'p']

    nouns = ["children", "people", "houses"]
    verbs = ["like", "hate", "play with", "hurt"]

    -- f nouns verbs
