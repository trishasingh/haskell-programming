module Apl1 where 

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- orphan instances for ZipList
instance Monoid a => Monoid (ZipList a) where 
  mempty = pure mempty

instance Semigroup a => Semigroup (ZipList a) where 
  (<>) = liftA2 (<>)

instance Eq a => EqProp (ZipList a) where (=-=) = eq 

-- List Applicative
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

-- instances
instance Functor List where
  fmap _ Nil = Nil  
  fmap f (Cons a as) = Cons (f a) (fmap f as) 

instance Applicative List where 
  pure = flip Cons Nil 
  _ <*> Nil = Nil 
  Nil <*> _ = Nil 
  -- (Cons f fs) <*> as = fmap f as `append` (fs <*> as)
  fs <*> as = flatMap (`fmap` as) fs

-- functions for List
append :: List a -> List a -> List a 
append Nil ys = ys 
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h $ fold f b t

concat' :: List (List a) -> List a 
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b 
flatMap f = concat' . fmap f

take' :: Int -> List a -> List a 
take' _ Nil = Nil
take' 0 xs = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)


-- Ziplist' Applicative
newtype ZipList' a = 
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where 
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs 
                in take' 3000 l 
          ys' = let (ZipList' l) = ys 
                in take' 3000 l 

instance Functor ZipList' where 
  fmap f (ZipList' xs) = ZipList' $ fmap f xs 

instance Applicative ZipList' where 
  pure xs = ZipList' $ repeat' xs
  (<*>) (ZipList' fs) (ZipList' xs) = 
    ZipList' $ zipWith' ($) fs xs 

instance Arbitrary a => Arbitrary (List a) where 
  arbitrary = Cons <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary
          

-- functions for ZipList
repeat' :: a -> List a 
repeat' a = Cons a Nil `append` repeat' a

zipWith' :: (a -> b -> c) 
         -> List a 
         -> List b 
         -> List c
zipWith' _ Nil _ = Nil 
zipWith' _ _ Nil = Nil 
zipWith' f (Cons x xs) (Cons y ys) =
  Cons (f x y) (zipWith' f xs ys)


main :: IO ()
main = quickBatch $ applicative (undefined :: ZipList' (Int, Double, String))


