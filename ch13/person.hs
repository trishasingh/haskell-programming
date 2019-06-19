{-# LANGUAGE ScopedTypeVariables #-}

module Person where 

import System.IO

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String 
                   deriving (Eq, Show)

mkPerson :: Name 
         -> Age 
         -> Either PersonInvalid Person 

mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty 
  | not (age > 0) = Left AgeTooLow 
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name 
                       ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do 
  hSetBuffering stdout NoBuffering
  putStrLn "Enter person's name: "
  name <- getLine
  putStrLn "Enter person's age: "
  age' <- getLine 
  let age = read age' :: Integer
  case (mkPerson name age) of 
    (Right (Person name age)) -> do
      putStrLn "Yay! Successfully got a person!"
      putStrLn $ show $ Person name age 
    (Left NameEmpty) -> 
      putStrLn "Error occured: name is empty."
    (Left AgeTooLow) -> 
      putStrLn "Error occured: age is too low."
    (Left (PersonInvalidUnknown s)) -> 
      putStrLn $ "Unknown error occured: " ++ s
      
    