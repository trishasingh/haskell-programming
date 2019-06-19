{-#LANGUAGE ScopedTypeVariables#-}

module Cipher where

import Data.Char
import Data.Bool
import System.IO

-- can add characters to alphabet
-- space added as example
alphabet = ['a'..'z'] ++ " "

alphaLen = length alphabet
alphaIndex = [0..alphaLen-1]
origMap = zip alphabet alphaIndex

-- convert character to index based on alphabet list
charToIndex :: [(Char, Int)] -> Char -> Int
charToIndex map c = head [snd x | x <- map, fst x == c]

-- convert index to character based on alphabet list
indexToChar :: [(Char, Int)] -> Int -> Char
indexToChar map i = head [fst x | x <- map, snd x == i]

-- enter text to be encoded and number of spaces to shift
shift :: String -> Int -> String
shift xs n = posToChar
    where positions  = map (charToIndex origMap) xs
          nShift     = mod n alphaLen
          shiftedPos = map (flip mod alphaLen . (+nShift)) 
                       positions
          posToChar  = map (indexToChar origMap) shiftedPos

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering 
    putStrLn "Please input a word to encrypt: "
    word :: String <- readLn
    putStrLn "Please specify how many spaces to shift: "
    n :: Int <- readLn
    let ciphered = shift (map toLower word) n
    let unciphered = shift ciphered (negate n)
    putStrLn $ "Ciphered word is: " ++ ciphered
    putStrLn $ "Unciphered word is: " ++ unciphered
