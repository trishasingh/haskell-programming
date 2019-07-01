module Optional where 

import Data.Monoid
import Test.QuickCheck

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> x = x
  x <> Nada = x
  (Only x) <> (Only y) = Only $ x <> y

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

-- Monoid instance for non-monoid contents
newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where 
  First' Nada <> x = x
  First' x <> _ = First' x

instance Monoid (First' a) where 
  mempty = First' Nada 

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do 
  x <- arbitrary
  frequency [ (3, return (First' (Only x)))
            , (1, return (First' Nada)) ]

firstMappend :: First' a 
             -> First' a
             -> First' a
firstMappend = (<>)

type FirstAssoc =
    First' String
 -> First' String
 -> First' String
 -> Bool

-- defining properties
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc x y z =
  x <> (y <> z) == (x <> y) <> z 

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x =
  x <> mempty == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity x =
  mempty <> x == x

-- writing checks
main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstAssoc)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)






