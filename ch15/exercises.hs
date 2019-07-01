module Exercises where

import Test.QuickCheck
import Data.Monoid

{-# ANN module "HLint: Use <$>" #-}

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  a <> (b <> c) == (a <> b) <> c 

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a =
  a <> mempty == a 

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a =
  mempty <> a == a

--1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type TrivialId = Trivial -> Bool

--2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where 
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool
type IdentityId a = Identity a -> Bool

 --3
data Two a b = 
  Two a b 
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where 
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary 
    return (Two a b)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool
type TwoId a b = Two a b -> Bool

--4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
  Semigroup (Three a b c) where
    (Three a1 b1 c1) <> (Three a2 b2 c2) = 
      Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Monoid a, Monoid b, Monoid c) =>
  Monoid (Three a b c) where
    mempty = Three mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return (Three a b c)

type ThreeAssoc a b c = 
    Three a b c 
 -> Three a b c 
 -> Three a b c 
 -> Bool
type ThreeId a b c = Three a b c -> Bool

--5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d) where
    (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = 
      Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance (Monoid a, Monoid b, Monoid c, Monoid d) =>
  Monoid (Four a b c d) where 
    mempty = Four mempty mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do 
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return (Four a b c d)

type FourAssoc a b c d = 
    Four a b c d 
 -> Four a b c d
 -> Four a b c d
 -> Bool
type FourId a b c d = Four a b c d -> Bool

 --6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj b1) <> (BoolConj b2) = BoolConj (b1 && b2)

instance Monoid BoolConj where 
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    a <- (arbitrary :: Gen Bool)
    return (BoolConj a)
    
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolConjId = BoolConj -> Bool

--7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj b1) <> (BoolDisj b2) = BoolDisj (b1 || b2)

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do 
    a <- (arbitrary :: Gen Bool)
    return (BoolDisj a)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type BoolDisjId = BoolDisj -> Bool

--8 
data Or a b = 
    Fst a
  | Snd b 
  deriving (Eq, Show)

instance Monoid a => Semigroup (Or a b) where
  x@(Snd _) <> _ = x
  (Fst x) <> (Fst y) = Fst (x <> y)
  _ <> y = y

instance Monoid a => Monoid (Or a b) where 
  mempty = Fst mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b] 

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool
type OrId a b = Or a b -> Bool

--9
newtype Combine a b =
  Combine {unCombine :: (a -> b)}

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ f <> g

instance Monoid b => Monoid (Combine a b) where 
  mempty = Combine (\_ -> mempty)

combineAssoc :: (Eq b, Semigroup b) => Fun a b -> Fun a b 
                                    -> Fun a b -> a -> Bool
combineAssoc (Fun _ f) (Fun _ g) (Fun _ h) a = 
    unCombine ((Combine f <> Combine g) <> Combine h) a 
 == unCombine (Combine f <> (Combine g <> Combine h)) a

combLeftIdentity :: (Eq b, Monoid b) => Fun a b -> a -> Bool 
combLeftIdentity (Fun _ f) a =
  unCombine (Combine f <> mempty) a == unCombine (Combine f) a
combRightIdentity :: (Eq b, Monoid b) => Fun a b -> a -> Bool 
combRightIdentity (Fun _ f) a =
  unCombine (mempty <> Combine f) a == unCombine (Combine f) a

type CombineAssoc a b =
    Fun a b 
 -> Fun a b
 -> Fun a b 
 -> a 
 -> Bool
type CombLeftId a b = Fun a b -> a -> Bool 
type CombRightId a b = Fun a b -> a -> Bool 

--10
newtype Comp a = 
  Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where 
  Comp f <> Comp g = Comp $ f . g 

instance Semigroup a => Monoid (Comp a) where
  mempty = Comp id 

compAssoc :: (Eq a, Semigroup a) => Fun a a -> Fun a a 
                                 -> Fun a a -> a -> Bool
compAssoc (Fun _ f) (Fun _ g) (Fun _ h) a = 
    unComp ((Comp f <> Comp g) <> Comp h) a 
 == unComp (Comp f <> (Comp g <> Comp h)) a

compLeftIdentity :: (Eq a, Semigroup a) => 
  Fun a a -> a -> Bool 
compLeftIdentity (Fun _ f) a =
  unComp (Comp f <> mempty) a == unComp (Comp f) a 

compRightIdentity :: (Eq a, Semigroup a) => 
  Fun a a -> a -> Bool
compRightIdentity (Fun _ f) a =
  unComp (mempty <> Comp f) a == unComp (Comp f) a

type CompAssoc a =
    Fun a a 
 -> Fun a a
 -> Fun a a 
 -> a 
 -> Bool
type CompLeftId a = Fun a a -> a -> Bool 
type CompRightId a = Fun a a -> a -> Bool

--11
data Validation' a b =
    Failure' a
  | Success' b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation' a b) where
  Success' s1 <> Success' s2 = Success' s1
  Failure' f1 <> Failure' f2 = Failure' $ f1 <> f2 
  _           <> Failure' f  = Failure' f 
  Failure' f  <> _           = Failure' f

instance (Arbitrary a, Arbitrary b) => 
  Arbitrary (Validation' a b) where
    arbitrary = do
      a <- arbitrary 
      b <- arbitrary
      elements [Failure' a, Success' b]

type ValidationAssoc a b = 
    Validation' a b 
 -> Validation' a b
 -> Validation' a b
 -> Bool

--12
newtype AccumulateRight a b =
  AccumulateRight (Validation' a b)
  deriving (Eq, Show)

instance Semigroup b => 
  Semigroup (AccumulateRight a b) where
    AccumulateRight (Success' s1) <> AccumulateRight (Success' s2) =
      AccumulateRight (Success' (s1 <> s2))
    AccumulateRight (Success' s) <> _ = AccumulateRight (Success' s)
    _ <> AccumulateRight (Success' s) = AccumulateRight (Success' s)
    AccumulateRight (Failure' f) <> _ = AccumulateRight (Failure' f)
    
instance (Arbitrary a, Arbitrary b) => 
  Arbitrary (AccumulateRight a b) where
    arbitrary = do
      v <- arbitrary
      return (AccumulateRight v)

type AccRightAssoc a b =
    AccumulateRight a b 
 -> AccumulateRight a b
 -> AccumulateRight a b 
 -> Bool

--13
newtype AccumulateBoth a b =
  AccumulateBoth (Validation' a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
  Semigroup (AccumulateBoth a b) where
    AccumulateBoth (Success' s1) <> AccumulateBoth (Success' s2) =
      AccumulateBoth (Success' (s1 <> s2))
    AccumulateBoth v1 <> AccumulateBoth v2 = 
      AccumulateBoth (v1 <> v2)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (AccumulateBoth a b) where
    arbitrary = do
      v <- arbitrary
      return (AccumulateBoth v)

type AccBothAssoc a b =
    AccumulateBoth a b
 -> AccumulateBoth a b 
 -> AccumulateBoth a b 
 -> Bool

-- mem monoid
newtype Mem s a =
  Mem { 
    runMem :: s -> (a,s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f1 <> Mem f2 = 
    Mem $ \s -> ( (fst . f1) s <> (fst . f2) s
                , (snd . f1 . snd . f2) s ) 

instance Monoid a => Monoid (Mem s a) where 
  mempty = Mem $ \s -> (mempty, s)

memLeftIdentity :: (Eq a, Eq s, Monoid a) => Fun s (a,s) -> s -> Bool 
memLeftIdentity (Fun _ f) s =
  runMem (Mem f <> mempty) s == runMem (Mem f) s 

memRightIdentity :: (Eq a, Eq s, Monoid a) => Fun s (a,s) -> s -> Bool 
memRightIdentity (Fun _ f) s =
  runMem (mempty <> Mem f) s == runMem (Mem f) s

type MemLeftId a s = Fun s (a,s) -> s -> Bool 
type MemRightId a s = Fun s (a,s) -> s -> Bool 

--main function
main :: IO ()
main = do
  -- semigroup checks
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (semigroupAssoc :: TwoAssoc String Ordering)
  quickCheck (semigroupAssoc :: ThreeAssoc String Ordering [Int])
  quickCheck (semigroupAssoc :: FourAssoc String Ordering [Int] [Double])
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc String Int)
  quickCheck (combineAssoc   :: CombineAssoc Int (Sum Int))
  quickCheck (compAssoc      :: CompAssoc String)
  quickCheck (semigroupAssoc :: ValidationAssoc String Int)
  quickCheck (semigroupAssoc :: AccRightAssoc Int String)
  quickCheck (semigroupAssoc :: AccBothAssoc [Int] String)
  -- monoid checks
  quickCheck (monoidLeftIdentity  :: TrivialId)
  quickCheck (monoidRightIdentity :: TrivialId)
  quickCheck (monoidLeftIdentity  :: IdentityId String)
  quickCheck (monoidRightIdentity :: IdentityId String)
  quickCheck (monoidLeftIdentity  :: TwoId String Ordering)
  quickCheck (monoidRightIdentity :: TwoId String Ordering)
  quickCheck (monoidLeftIdentity  :: ThreeId String Ordering [Int])
  quickCheck (monoidRightIdentity :: ThreeId String Ordering [Int])
  quickCheck (monoidLeftIdentity  :: FourId String Ordering [Int] [Double])
  quickCheck (monoidRightIdentity :: FourId String Ordering [Int] [Double])
  quickCheck (monoidLeftIdentity  :: BoolConjId)
  quickCheck (monoidRightIdentity :: BoolConjId)
  quickCheck (monoidLeftIdentity  :: BoolDisjId)
  quickCheck (monoidRightIdentity :: BoolDisjId)
  quickCheck (monoidLeftIdentity  :: OrId String Int)
  quickCheck (monoidRightIdentity :: OrId String Int)
  quickCheck (combLeftIdentity    :: CombLeftId Int (Sum Int))
  quickCheck (combRightIdentity   :: CombRightId Int (Sum Int))
  quickCheck (compLeftIdentity    :: CompLeftId String)
  quickCheck (compLeftIdentity    :: CompRightId String)
  quickCheck (memLeftIdentity     :: MemLeftId String Int)
  quickCheck (memRightIdentity    :: MemRightId String Int)
