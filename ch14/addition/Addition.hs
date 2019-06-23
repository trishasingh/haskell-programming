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
    it "1 + 1 is greater than 1" $ 
      (1 + 1) > 1 `shouldBe` True 
    it "2 + 2 is equal to 4" $
      2 + 2 `shouldBe` 4 
    it "x + 1 is always greater than x" $
      property $ \x -> x + 1 > (x :: Int)
  describe "Division" $ do 
    it "15 divided by 3 is 5" $
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $
      dividedBy 22 5 `shouldBe` (4, 2)
  describe "Multiplication" $ do 
    it "15 multiplied by 3 is 15" $ 
        multipliedBy 15 3 `shouldBe` 45
    it "2 multiplied by 0 is 0" $ 
        multipliedBy 2 0 `shouldBe` 0

-- QuickCheck without Hspec
prop_additionGreater :: Int -> Bool 
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

-- NOTES/Playing around:

-- random sampling
x = sample (arbitrary :: Gen Int)
x' = sample' (arbitrary :: Gen Int)

trivial :: Gen Int
trivial = return 1

oneThroughThree :: Gen Int 
oneThroughThree = elements [1,2,3]

oneThroughThree' :: Gen Int 
oneThroughThree' = elements [1,2,2,2,2,3]

-- more examples
genBool :: Gen Bool 
genBool = choose (True, False)

genBool' :: Gen Bool 
genBool' = elements [True, False]

genOrdering :: Gen Ordering 
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char 
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do 
  a <- arbitrary
  b <- arbitrary
  return (a, b)
--sample (genTuple :: Gen(Int, Float))

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) 
            => Gen (a, b, c)
genThreeple = do 
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do 
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do 
  a <- arbitrary
  elements [Nothing, Just a]

-- using frequency for sampling probabilities
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do 
  a <- arbitrary
  frequency [(1, return Nothing),
             (3, return (Just a))]

