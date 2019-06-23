module Hangman where

import Data.List (intersperse)

{-# ANN renderPuzzleChar "HLint: ignore Use fromMaybe" #-}
    
-- DATA TYPES

newtype Wordlist = 
    Wordlist [String]
    deriving (Eq, Show)
-- make the puzzle
data Puzzle = Puzzle String [Maybe Char] [Char] Int deriving Eq

-- generate master word list
-- loading in dict
allWords :: IO Wordlist 
allWords = do
    dict <- readFile "data/dict.txt"
    return $ Wordlist (lines dict)

-- setting word limits
minWordLength :: Int 
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9 

-- filtering dictionary based on word length
gameWords :: IO Wordlist 
gameWords = do 
    (Wordlist aw) <- allWords 
    return $ Wordlist (filter gameLength aw) 
    where gameLength w = 
            let l = length (w :: String)
            in l > minWordLength && l < maxWordLength

-- setting incorrect guess limit
guessLimit :: Int 
guessLimit = 7

-- show the puzzle to the user 
instance Show Puzzle where 
  show (Puzzle _ discovered guessed incorrect) =
      intersperse ' ' (fmap renderPuzzleChar discovered)
      ++ "\nGuessed so far: " ++ guessed 
      ++ "\n" ++ show (guessLimit - incorrect)
      ++ " incorrect guesses left!\n"

-- used in show instance of puzzle
renderPuzzleChar :: Maybe Char -> Char 
renderPuzzleChar = maybe '_' id

-- HELPER FUNCTIONS

freshPuzzle :: String -> Puzzle 
freshPuzzle s = Puzzle s discovered [] 0
    where discovered = map (const Nothing) s

-- check if guessed character is in puzzle
charInWord :: Puzzle -> Char -> Bool 
charInWord (Puzzle word _ _ _) c = 
    c `elem` word

-- check if character is already guessed
alreadyGuessed :: Puzzle -> Char -> Bool 
alreadyGuessed (Puzzle _ _ guessed _) c =
    c `elem` guessed 

-- add to tally of incorrect guesses
penalize :: Puzzle -> Puzzle 
penalize (Puzzle word discovered guessed incorrect) =
    Puzzle word discovered guessed (incorrect + 1)
   
-- fill in guessed character into puzzle
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s incorrect) c =
    Puzzle word newFilledInSoFar (c : s) incorrect
    where zipper guessed wordChar guessChar = 
            if wordChar == guessed 
            then Just wordChar 
            else guessChar 
          newFilledInSoFar =
            zipWith (zipper c) word filledInSoFar

-- handle the user's guess
handleGuess :: Puzzle -> Char -> IO Puzzle 
handleGuess puzzle guess = do
    putStrLn $ "Your guess was " ++ [guess]
    case (charInWord puzzle guess
        , alreadyGuessed puzzle guess) of
      (_, True) -> do 
        putStrLn "You already guessed that\
                  \ character, pick something else!"
        return puzzle
      (True, _) -> do 
        putStrLn "This character was in the word,\
                  \ filling in the word accordingly."
        return (fillInCharacter puzzle guess)
      (False, _) -> do 
        putStrLn "This character wasn't in\
                  \ the word, try again."
        return (penalize $ fillInCharacter puzzle guess)