module Main where 

import qualified Data.Map as M 
import Morse 
import Test.QuickCheck 

-- set up generators 
allowedChars :: [Char]
allowedChars = M.keys letterToMorse 

allowedMorse :: [String]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char 
charGen = elements allowedChars

morseGen :: Gen String 
morseGen = elements allowedMorse

prop_thereAndBack :: Property
prop_thereAndBack =
  forAll charGen
  (\c -> (charToMorse c >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck prop_thereAndBack
