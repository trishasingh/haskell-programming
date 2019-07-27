module TypeCheckEx where

import Control.Applicative
import Data.List (elemIndex)

-- Short exercises p 787

--1
added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1..3] [5..7])

--2
y :: Maybe Integer 
y = lookup 3 $ zip [1..3] [4..6]

z :: Maybe Integer 
z = lookup 2 $ zip [1..3] [4..6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

--3
x' :: Maybe Int 
x' = elemIndex 3 [1..5]

y' :: Maybe Int 
y' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int 
max' = max 

maxed :: Maybe Int 
maxed = max' <$> x' <*> y'

--4
xs = [1..3]
ys = [4..6]

x'' :: Maybe Integer 
x'' = lookup 3 $ zip xs ys 

y'' :: Maybe Integer 
y'' = lookup 2 $ zip xs ys 

summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')

-- pg 806
f1 = const <$> Just "Hello" <*> pure "World"
f1' = liftA2 const (Just "Hello") (pure "World")
f2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]