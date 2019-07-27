module ChapterExercises where 

import Data.Monoid 
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

-- Specialized Types
pureList :: a -> [a]
pureList = pure
apList :: [a -> b] -> [a] -> [b]
apList = (<*>)

pureIO :: a -> IO a 
pureIO = pure
apIO :: IO (a -> b) -> IO a -> IO b 
apIO = (<*>)

pureTuple :: Monoid a => a -> (a, a)
pureTuple = pure
apTuple :: Monoid a 
        => (a, b -> c) 
        -> (a, b)
        -> (a, c)
apTuple = (<*>)

pureFuncCons :: a -> (b -> a)
pureFuncCons = pure 
apFuncCons :: (a -> b -> c) -> (a -> b) -> (a -> c)
apFuncCons = (<*>) 


-- Applicative Instances 
-- Identity
newtype Identity a = Identity a deriving Show 

instance Functor Identity where 
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where 
  pure = Identity
  (<*>) (Identity f) = fmap f

instance Arbitrary a => Arbitrary (Identity a) where 
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where 
  (Identity x) =-= (Identity y) = x `eq` y

-- Pair 
data Pair a = Pair a a deriving Show 

instance Functor Pair where 
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where 
  pure x = Pair x x
  (<*>) (Pair f0 f1) (Pair x0 x1) = Pair (f0 x0) (f1 x1)

instance Arbitrary a => Arbitrary (Pair a) where 
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where 
  (Pair x0 y0) =-= (Pair x1 y1) = 
    property $ (x0==x1) && (y0==y1)

-- Two
data Two a b =
  Two a b 
  deriving (Eq, Show)

instance Functor (Two a) where 
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where 
  pure = Two mempty
  (<*>) (Two m0 f) (Two m1 x) = Two (m0 <> m1) (f x)

instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (Two a b) where 
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where 
  (Two x0 y0) =-= (Two x1 y1) = 
    property $ (x0 == x1) && (y0 == y1)

-- Three
data Three a b c =
  Three a b c
  deriving Show

instance Functor (Three a b) where 
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where 
  pure = Three mempty mempty
  (<*>) (Three x0 y0 f) (Three x1 y1 z) = 
    Three (x0 <> x1) (y0 <> y1) (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
          Arbitrary (Three a b c) where 
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where 
  (Three x0 y0 z0) =-= (Three x1 y1 z1) = 
    property $ (x0 == x1) && (y0 == y1) && (z0 == z1)

-- Three'
data Three' a b =
  Three' a b b 
  deriving Show 

instance Functor (Three' a) where 
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where 
  pure x = Three' mempty x x
  (<*>) (Three' x0 f g) (Three' x1 y z) =
    Three' (x0 <> x1) (f y) (g z)

instance (Arbitrary a, Arbitrary b) => 
          Arbitrary (Three' a b) where 
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where 
  (Three' x0 y0 z0) =-= (Three' x1 y1 z1) =
    property $ (x0 == x1) && (y0 == y1) && (z0 == z1)

-- Four
data Four a b c d =
  Four a b c d 
  deriving Show 

instance Functor (Four a b c) where 
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) =>
          Applicative (Four a b c) where 
  pure = Four mempty mempty mempty
  (<*>) (Four a0 b0 c0 f) (Four a1 b1 c1 d) =
    Four (a0 <> a1) (b0 <> b1) (c0 <> c1) (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
          Arbitrary (Four a b c d) where 
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) =>
          EqProp (Four a b c d) where 
  (Four a0 b0 c0 d0) =-= (Four a1 b1 c1 d1) =
    property $ (a0==a1) && (b0==b1) && (c0==c1) && (d0==d1)

-- Four'
data Four' a b =
  Four' a a a b
  deriving Show 

instance Functor (Four' a) where 
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where 
  pure = Four' mempty mempty mempty
  (<*>) (Four' a0 b0 c0 f) (Four' a1 b1 c1 d) =
    Four' (a0<>a1) (b0<>b1) (c0<>c1) (f d)

instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (Four' a b) where 
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where 
  (Four' a0 b0 c0 d0) =-= (Four' a1 b1 c1 d1) =
    property $ (a0==a1) && (b0==b1) && (c0==c1) && (d0==d1)


main :: IO ()
main =
  --quickBatch $ applicative (undefined :: Identity (Int, String, Double))
  --quickBatch $ applicative (undefined :: Pair (Int, String, Double))
  --quickBatch $ applicative (undefined :: Two String (Int, String, Double))
  --quickBatch $ applicative (undefined :: Three String String (Int, String, Double))
  -- quickBatch $ applicative (undefined :: Three' String (Int, String, Double))
  --quickBatch $ applicative (undefined :: Four String String String (Int, String, Double))
  quickBatch $ applicative (undefined :: Four' String (Int, Double, String))



