module Palindrome where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char (toLower)

removePunct :: String -> String
removePunct = filter (\a -> elem a ['a'..'z'])

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let line2 = (removePunct . map toLower) line1
  case (line2 == reverse line2) of 
    True  -> do putStrLn "It's a palindrome!"
                return ()
    False -> do putStrLn "Nope!"
                exitSuccess
