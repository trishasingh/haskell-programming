module AsPatterns where

    import Data.Char

    isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
    isSubsequenceOf sub@(x:xs) super
        | sub == [] = True
        | otherwise = (elem x super) 
                    && isSubsequenceOf xs super

    capitalizeWords :: String -> [(String, String)]
    capitalizeWords sentence = map capitalize (words sentence)
        where capitalize = \wrd@(x:xs) -> (wrd, (toUpper x) : xs)


