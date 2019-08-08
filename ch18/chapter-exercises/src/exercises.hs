{-HLINT ignore "Use =<<"-}
{-HLINT ignore "Use join"-}

module Exercises where 

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

-- 1
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where 
  pure x = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where 
  return = pure 
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where 
  arbitrary = elements [NopeDotJpg]

instance EqProp (Nope a) where (=-=) = eq

-- 2
data Either' b a = 
    Left' a 
  | Right' b 
  deriving (Eq, Show)

instance Functor (Either' b) where 
  fmap f (Left' a) = Left' $ f a 
  fmap _ (Right' b) = Right' b 

instance Applicative (Either' b) where 
  pure = Left' 
  (Right' b) <*> _ = Right' b 
  _ <*> (Right' b) = Right' b 
  (Left' f) <*> (Left' a) = Left' $ f a 

instance Monad (Either' b) where 
  return = pure 
  (Right' b) >>= _ = Right' b 
  (Left' a) >>= f = f a

instance (Arbitrary a, Arbitrary b) => 
          Arbitrary (Either' b a) where 
  arbitrary = do
    b <- arbitrary
    a <- arbitrary
    elements [Left' a, Right' b]

instance (Eq a, Eq b) => EqProp (Either' b a) where 
  (=-=) = eq

-- 3 
newtype Identity a = Identity a 
    deriving (Eq, Ord, Show)

instance Functor Identity where 
  fmap f (Identity x) = Identity $ f x 

instance Applicative Identity where 
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x 

instance Monad Identity where 
  return = pure 
  (Identity x) >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where 
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- 4
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where 
  fmap _ Nil = Nil 
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where 
  pure x = Cons x Nil 
  Nil <*> _ = Nil 
  _ <*> Nil = Nil 
  (Cons f fs) <*> xs = fmap f xs `append` (fs <*> xs)

instance Monad List where 
  return = pure 
  Nil >>= _ = Nil 
  xs >>= f = join $ fmap f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do 
    x <- arbitrary
    y <- arbitrary
    elements [Nil, Cons x Nil, Cons x y]

instance Eq a => EqProp (List a) where (=-=) = eq

append :: List a -> List a -> List a 
append Nil ys = ys 
append (Cons x xs) ys = Cons x $ xs `append` ys

join :: List (List a) -> List a 
join (Cons Nil Nil) = Nil 
join (Cons x Nil) = x
join (Cons x xs) = x `append` join xs


main :: IO ()
main = do 
  quickBatch $ monad (undefined :: Nope (String, String, String))
  quickBatch $ monad (undefined :: Either' String (String, String, String))
  quickBatch $ monad (undefined :: Identity (String, String, String))
  quickBatch $ monad (undefined :: List (String, String, String))


-- rewrite functions using monad and functor methods

-- 1
j :: Monad m => m (m a) -> m a 
j m = m >>= id

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b 
l1 f m = m >>= \x -> return (f x)

-- 3 
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c 
l2 f m m' = m >>= \x -> f x <$> m'

-- 4
a :: Monad m => m a -> m (a -> b) -> m b 
a m f = f >>= \x -> x <$> m 

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = f a >>= \x -> fmap (x:) (meh as f)

-- 6
flipType :: (Monad m) => [m a] -> m [a]
flipType m = meh m id
