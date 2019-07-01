{-#LANGUAGE ScopedTypeVariables#-}

module VigenereCipher where

import Data.Char
import Data.Bool
import System.IO

-- ALPHABET: CAN MODIFY BASED ON WHAT INPUT
--           IS ALLOWED TO CONTAIN
-- can add characters to alphabet
-- space added as example
alphabet = ['a'..'z'] ++ " "
alphaLen = length alphabet
alphaIndex = [0..alphaLen-1]
origMap = zip alphabet alphaIndex

-- HELPER FUNCTIONS
-- convert character to index based on alphabet list
charToIndex :: [(Char, Int)] -> Char -> Int
charToIndex map c = head [snd x | x <- map, fst x == c]

-- convert index to character based on alphabet list
indexToChar :: [(Char, Int)] -> Int -> Char
indexToChar map i = head [fst x | x <- map, snd x == i]

-- fit the key to the input value
expand :: Int -> String -> String
expand n key = take n $ (concat . replicate times) key
    where times = (+1) $ div n (length key)

-- enter text to be encoded and number of spaces to shift
-- shift :: String -> Int -> String
shift xs key = posToChar
    where nShift = map (charToIndex origMap) 
                   (expand (length xs) key)
          positions = map (charToIndex origMap) xs
          shiftedPos = zipWith (\a b -> mod (a+b) alphaLen) 
                       positions nShift
          posToChar  = map (indexToChar origMap) shiftedPos

-- MAIN FUNCTION
main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    putStrLn "Please input a word to encrypt: "
    word :: String <- readLn
    putStrLn "Please a key to use for encryption: "
    key :: String <- readLn 
    putStrLn $ "Ciphered word is: " 
             ++ shift (map toLower word) key
