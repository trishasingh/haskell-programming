module ValidationEx where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- Sum and Validation types
-- both are natural transformations of Either
data Sum' a b =
    First' a 
  | Second' b 
  deriving (Eq, Show)

data Validation' e a =
    Error' e 
  | Success' a 
  deriving (Eq, Show)

-- Sum instances
instance Functor (Sum' a) where 
  fmap _ (First' a) = First' a 
  fmap f (Second' b) = Second' $ f b 

instance Applicative (Sum' a) where 
  pure = Second'
  (Second' f) <*> (Second' x) = Second' $ f x 
  (First' f) <*> _ = First' f 
  _ <*> (First' x) = First' x 

instance (Eq a, Eq b) => EqProp (Sum' a b) where 
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => 
          Arbitrary (Sum' a b) where 
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    elements [First' a, Second' b]


-- Validation instances 
instance Functor (Validation' e) where 
  fmap _ (Error' e) = Error' e 
  fmap f (Success' s) = Success' $ f s 

instance Monoid e => 
         Applicative (Validation' e) where
  pure = Success' 
  (Error' e0) <*> (Error' e1) = Error' $ e0 <> e1
  (Error' e) <*> _ = Error' e 
  _ <*> (Error' e) = Error' e 
  (Success' f) <*> (Success' x) = Success' $ f x 

instance (Eq e, Eq s) => EqProp (Validation' e s) where 
  (=-=) = eq 

instance (Arbitrary e, Arbitrary s) => 
          Arbitrary (Validation' e s) where 
  arbitrary = do 
    e <- arbitrary
    s <- arbitrary
    elements [Error' e, Success' s]

main :: IO ()
main = do
  quickBatch $ applicative (undefined :: Sum' Int (String, Int, Double))
  quickBatch $ applicative (undefined :: Validation' String (String, Int, Double))


