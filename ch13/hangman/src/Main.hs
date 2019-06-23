module Main where

import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import Control.Monad (forever)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Hangman

{-# ANN module "HLint: ignore Use when" #-}
{-# ANN module "HLint: ignore Use String" #-}

-- choose a random word from our game word list 
randomWord :: Wordlist -> IO String 
randomWord (Wordlist wl) = do 
    randomIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randomIndex 

randomWord' :: IO String 
randomWord' = gameWords >>= randomWord

-- end the game
gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ incorrect) =
    if incorrect >= guessLimit then
      do putStrLn "You lose!"
         putStrLn $ "The word was " ++ wordToGuess
         exitSuccess 
    else return ()
    
-- exit the game if you win
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) = 
    if all isJust filledInSoFar then 
      do putStrLn "You win!"
         exitSuccess
    else return ()
    
-- running the game 
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do 
    gameOver puzzle
    gameWin puzzle 
    putStrLn $ "Current puzzle is: " ++ show puzzle 
    putStr "Guess a letter: "
    guess <- getLine
    case guess of 
      [c] -> handleGuess puzzle c >>= runGame
      _   -> putStrLn "Your guess must\
                       \ be a single character."

main :: IO ()
main = do 
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle 


