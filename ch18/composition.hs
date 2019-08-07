module Composition where

import Control.Monad ((>=>))

-- Kleisli composition
mcomp :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c 
mcomp f g a = f a >>= g 

-- example
sayHi :: String -> IO String 
sayHi greeting = do 
  putStrLn greeting 
  getLine

readM :: Read a => String -> IO a 
readM = return . read 

getAge :: String -> IO Int 
getAge = sayHi >=> readM

askForAge :: IO Int 
askForAge = getAge "Hello! How old are you?"
