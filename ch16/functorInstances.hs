module FunctorInstances where 

import Test.QuickCheck

{-# ANN module "Hlint: Use <$>" #-}

-- Functor law checks
functorId :: (Functor f, Eq (f a)) => 
  f a -> Bool 
functorId x = 
  fmap id x == x

type FunctorId a = a -> Bool 

functorComp :: (Functor f, Eq (f c)) =>
  Fun a b -> Fun b c -> f a -> Bool 
functorComp (Fun _ f) (Fun _ g) x =
  fmap (g . f) x == (fmap g . fmap f) x

type IntStringComp a = 
    Fun Int String -> Fun String Int
 -> a -> Bool

--1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where 
  fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where 
  arbitrary = Identity <$> arbitrary

type IdentityTest = Identity Int

--2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a0 a1) = Pair (f a0) (f a1) 

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do 
    a0 <- arbitrary
    a1 <- arbitrary
    return $ Pair a0 a1

type PairTest = Pair Int 

--3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Two a b) where
    arbitrary = do 
      a <- arbitrary 
      b <- arbitrary
      return (Two a b)

type TwoTest = Two String Int

--4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
  
instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where 
    arbitrary = do 
      a <- arbitrary 
      b <- arbitrary
      c <- arbitrary
      return $ Three a b c 

type ThreeTest = Three String Double Int

--5
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where 
  fmap f (Three' a b0 b1) = Three' a (f b0) (f b1)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Three' a b) where 
    arbitrary = do 
      a <- arbitrary
      b0 <- arbitrary
      b1 <- arbitrary
      return $ Three' a b0 b1 

type Three'Test = Three' String Int

--6
data Four a b c d = 
  Four a b c d 
  deriving (Eq, Show)

instance Functor (Four a b c) where 
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where 
    arbitrary = do 
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four a b c d 

type FourTest = Four String Double Ordering Int 

--7
data Four' a b = Four' a a b b deriving (Eq, Show)

instance Functor (Four' a) where 
  fmap f (Four' a0 a1 b0 b1) = Four' a0 a1 (f b0) (f b1)

instance (Arbitrary a, Arbitrary b) => 
  Arbitrary (Four' a b) where
    arbitrary = do 
      a0 <- arbitrary
      a1 <- arbitrary
      b0 <- arbitrary
      b1 <- arbitrary
      return $ Four' a0 a1 b0 b1 

type Four'Test = Four' String Int 

main :: IO ()
main = do
  quickCheck (functorId   :: FunctorId IdentityTest)
  quickCheck (functorComp :: IntStringComp IdentityTest)
  quickCheck (functorId   :: FunctorId PairTest)
  quickCheck (functorComp :: IntStringComp PairTest)
  quickCheck (functorId   :: FunctorId TwoTest)
  quickCheck (functorComp :: IntStringComp TwoTest)
  quickCheck (functorId   :: FunctorId ThreeTest)
  quickCheck (functorComp :: IntStringComp ThreeTest)
  quickCheck (functorId   :: FunctorId Three'Test)
  quickCheck (functorComp :: IntStringComp Three'Test)
  quickCheck (functorId   :: FunctorId FourTest)
  quickCheck (functorComp :: IntStringComp FourTest)
  quickCheck (functorId   :: FunctorId Four'Test)
  quickCheck (functorComp :: IntStringComp Four'Test)