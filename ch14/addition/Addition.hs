module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
  where go n d count 
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

multipliedBy :: (Eq a, Num a) => a -> a -> a 
multipliedBy a b = go 0 a b
  where go n a b
         | b == 0 = n 
         | otherwise = go (n + a) a (b - 1)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do 
    it "1 + 1 is greater than 1" $ do 
      (1 + 1) > 1 `shouldBe` True 
    it "2 + 2 is equal to 4" $ do 
      2 + 2 `shouldBe` 4 
    it "x + 1 is always greater than x" $ do 
      property $ \x -> x + 1 > (x :: Int)
  describe "Division" $ do 
    it "15 divided by 3 is 5" $ do 
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
  describe "Multiplication" $ do 
    it "15 multiplied by 3 is 15" $ do 
        multipliedBy 15 3 `shouldBe` 45
    it "2 multiplied by 0 is 0" $ do 
        multipliedBy 2 0 `shouldBe` 0