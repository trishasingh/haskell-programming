module StringProcessing where 

    import Data.Bool 
    import Data.Char
    import Data.List

    notThe :: String -> Maybe String
    notThe s = bool (Just s) Nothing (s=="the")

    replaceThe :: String -> String
    replaceThe s = concat . intersperse " " $ xs
        where xs = map f $ map notThe (words s)
              f x = case x of 
                        Nothing     -> "a" 
                        (Just word) -> word

    vowels = "aeiou"

    isVowel :: Char -> Bool 
    isVowel = flip elem "aeiou"
     
    test = "the house the arm ask"

    countTheBeforeVowel :: String -> Integer
    countTheBeforeVowel = countTheBeforeVowel' . words

    countTheBeforeVowel' :: [String] -> Integer
    countTheBeforeVowel' s@(x:xs)
        | s == [] = 0
        | xs == [] = 0
        | otherwise 
            = bool 0 1 (checkThe x && checkVowel (head xs)) 
            + countTheBeforeVowel' xs
        where checkThe a = if (notThe a == Nothing)
                           then True else False
              checkVowel a = isVowel (head a) 

    countVowels :: String -> Integer 
    countVowels [] = 0
    countVowels (x:xs) = bool 0 1 (isVowel . toLower $ x) 
                       + countVowels xs

    -- Validate word exercise
    newtype Word' =
        Word' String
        deriving (Eq, Show)

    mkWord :: String -> Maybe Word'
    mkWord s = bool Nothing (Just (Word' s)) (tally > 0)
       where tally = foldr (\a b -> mkTally a + b) 0 s
             mkTally a = bool 1 (-1) (isVowel a)

    

