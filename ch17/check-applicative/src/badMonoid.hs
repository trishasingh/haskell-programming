module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

data Bull =
    Fools
  | Twoo 
  deriving (Eq, Show)

instance Arbitrary Bull where 
  arbitrary =
    elements [Fools, Twoo]

instance Monoid Bull where 
  mempty = Fools 

instance Semigroup Bull where 
  _ <> _ = Fools

instance EqProp Bull where 
  (=-=) = eq 

main :: IO ()
main = do
  quickBatch $ monoid Twoo
  quickBatch $ applicative [("b", "w", 2 :: Int)]
  quickBatch $ applicative [undefined :: (String, String, Int)]


