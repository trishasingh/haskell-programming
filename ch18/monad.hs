module Monad where

import Control.Monad (join)
import Control.Applicative ((*>))

{- HLINT ignore "Use =<<" -}

-- pg 836
bind :: Monad m => (a -> m b) -> m a -> m b 
bind f = join . fmap f

-- pg 841
sequencing :: IO ()
sequencing = do 
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO () 
sequencing' = 
  putStrLn "blah" >>
  putStrLn "another thing"

sequencing'' :: IO () 
sequencing'' = 
  putStrLn "blah" *>
  putStrLn "another thing"

binding :: IO () 
binding = do 
  name <- getLine 
  putStrLn name 

binding' :: IO ()
binding' = 
  getLine >>= putStrLn

-- pg 845
bindingAndSequencing :: IO () 
bindingAndSequencing = do 
  putStrLn "name pls:"
  name <- getLine
  putStrLn $ "y hello thar " ++ name

bindingAndSequencing' :: IO () 
bindingAndSequencing' = 
  putStrLn "name pls:" >>
  getLine >>= 
  \name -> putStrLn $ "y hello thar " ++ name

twoBinds :: IO ()
twoBinds = do 
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls"
  age <- getLine
  putStrLn ("y hello thar: "
            ++ name ++ " who is: "
            ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' = 
  putStrLn "name pls:" >>
  getLine >>= 
    \name -> 
      putStrLn "age pls:" >>
      getLine >>=
        \age -> 
          putStrLn ("y hello thar "
                    ++ name ++ " who is: "
                    ++ age ++ " years old.")

-- pg 848
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do 
  x <- xs 
  if even x 
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do 
  x <- xs 
  if even x 
    then [x*x, x*x]
    else []
