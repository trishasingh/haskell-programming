module Qc where

import Test.QuickCheck
import Data.Monoid 
import Control.Monad

-- properties to test
prop_assoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
prop_assoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

prop_leftIdentity :: (Eq m, Monoid m) => m -> Bool
prop_leftIdentity a = 
  (a <> mempty) == a 

prop_rightIdentity :: (Eq m, Monoid m) => m -> Bool 
prop_rightIdentity a =
  (mempty <> a) == a 

-- new type to test
data Bull = 
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where 
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where 
  mempty = Fools 

instance Semigroup Bull where 
  _ <> _ = Fools 

type BullAssoc = Bull -> Bull -> Bull -> Bool

-- abbreviations for existing types
type S = String
type B = Bool

main :: IO ()
main = do
  quickCheck (prop_assoc :: S -> S -> S -> B)
  quickCheck (prop_leftIdentity :: S -> B)
  quickCheck (prop_rightIdentity :: S -> B)
  quickCheck (prop_assoc :: BullAssoc)
  quickCheck (prop_leftIdentity :: Bull -> Bool)
  quickCheck (prop_rightIdentity :: Bull -> Bool)