module LanguageExercises where

    import Data.Char
    import Data.Bool

    capitalizeWord :: String -> String
    capitalizeWord (x:xs) = toUpper x : xs

    capitalizeParagraph :: String -> String
    capitalizeParagraph para = concat $ [a] 
                             ++ map (". "++) as 
                             ++ ["."]
        where xs@(a:as) = map capitalizeWord (split' '.' para)

    split' :: Char -> String -> [String]
    split' _ [] = []
    split' p xs = [first] ++ split' p rest
        where first  = takeWhile (/=p) first'
              first' = bool xs (tail xs) (isSpace (head xs))
              rest   = tail $ dropWhile (/=p) xs

              
