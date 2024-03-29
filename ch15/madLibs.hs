module MadLibs where

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String 

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  mconcat [e, "! he said ",
           adv, " as he jumped into his car ",
           noun, " and drove off with his ",
           adj, " wife."]

main :: IO ()
main = do 
  putStrLn "Enter excalamation: "
  e <- getLine
  putStrLn "Enter adverb: "
  adv <- getLine 
  putStrLn "Enter noun: "
  noun <- getLine 
  putStrLn "Enter adjective: "
  adj <- getLine 
  putStrLn $ madlibbin' e adv noun adj
