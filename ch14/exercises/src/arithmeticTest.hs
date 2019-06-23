module ArithmeticTest where

import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper, toLower)

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Use String" #-}

-- 1
half :: Fractional a => a -> a
half x = x/2

halfIdentity :: Fractional a => a -> a 
halfIdentity = (*2) . half 

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = halfIdentity x == x

qc_halfIdentity :: IO ()
qc_halfIdentity = do 
  quickCheck (prop_halfIdentity :: Float -> Bool)
  quickCheck (prop_halfIdentity :: Double -> Bool)

-- 2
listOrdered :: (Ord a) => [a] -> Bool 
listOrdered xs = snd $ foldr go (Nothing, True) xs 
  where go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool 
prop_listOrdered = listOrdered . sort 

qc_listOrdered = do 
    quickCheck (prop_listOrdered :: [Int] -> Bool)
    quickCheck (prop_listOrdered :: [Ordering] -> Bool)
    quickCheck (prop_listOrdered :: [Char] -> Bool)

-- 3
plusAssociative x y z = 
    (x + y) + z == x + (y + z)
plusCommutative x y =
    x + y == y + x 

qc_plusProps = do 
    quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
    quickCheck (plusCommutative :: Int -> Int -> Bool)

-- 4
multAssociative x y z =
    (x * y) * z == (x * (y * z))
multCommutative x y =
    x * y == y * x

qc_multProps = do 
    quickCheck (multAssociative :: Int -> Int -> Int -> Bool)
    quickCheck (multCommutative :: Int -> Int -> Bool)

-- 5 
prop_quotRem x y =
  y == 0 || quot x y * y + rem x y == x
prop_divMod x y =
  y == 0 || div x y * y + mod x y == x

qc_quotRules = do 
  quickCheck (prop_quotRem :: Int -> Int -> Bool)
  quickCheck (prop_divMod :: Int -> Int -> Bool)

-- 6
prop_expAssociative x y z =
  (x ^ y) ^ z == x ^ (y ^ z)
prop_expCommutative x y =
  x ^ y == y ^ x 

qc_expProps = do 
  quickCheck (prop_expAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (prop_expCommutative :: Int -> Int -> Bool)

-- 7 
doubleRev = reverse . reverse 
prop_revIdentity xs = doubleRev xs == id xs

qc_revProps = do 
  quickCheck (prop_revIdentity :: [Int] -> Bool)
  quickCheck (prop_revIdentity :: [Char] -> Bool)

-- 8 
prop_apply (Fun _ f) x =
  (f $ x) == f x
prop_compose (Fun _ f) (Fun _ g) x = 
  (f . g) x == f (g x)

qc_otherFuncs = do 
  quickCheck (prop_apply :: Fun Int Int -> Int -> Bool)
  quickCheck (prop_compose :: Fun Int Int -> Fun Int Int -> Int -> Bool)

-- 9
prop_cons :: Eq a => [a] -> [a] -> Bool
prop_cons xs ys =
  foldr (:) xs ys == (++) ys xs
prop_concat :: Eq a => [[a]] -> Bool
prop_concat xs = 
  foldr (++) [] xs == concat xs 

qc_joinLists = do 
  quickCheck (prop_cons :: String -> String -> Bool)
  quickCheck (prop_cons :: [Int] -> [Int] -> Bool)
  quickCheck (prop_concat :: [String] -> Bool)
  quickCheck (prop_concat :: [[Int]] -> Bool)

-- 10
prop_lengthIdentity :: Eq a => Positive Int -> [NonEmptyList a] -> Bool
prop_lengthIdentity (Positive n) xs = n > length xs 
                                   || length (take n xs) == n 

qc_lengthIdentity = do 
  quickCheck (prop_lengthIdentity :: Positive Int -> [NonEmptyList Char] -> Bool)
  quickCheck (prop_lengthIdentity :: Positive Int -> [NonEmptyList Int] -> Bool)

-- 11 
prop_readShow x =
  read (show x) == x 

qc_readShow = do 
  quickCheck (prop_readShow :: Int -> Bool)
  quickCheck (prop_readShow :: [Double] -> Bool)
  quickCheck (prop_readShow :: String -> Bool)

-- 12
square x = x * x
prop_sqIdentity (Positive x) = 
  x == (square . sqrt) x

qc_sqIdentity = do
  quickCheck (prop_sqIdentity :: Positive Float -> Bool)
  quickCheck (prop_sqIdentity :: Positive Double -> Bool)

-- 13
twice f = f . f 
fourTimes = twice . twice 

capitalizeWord :: String -> String 
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : map toLower xs

prop_capIdem xs = capitalizeWord xs == twice capitalizeWord xs
               && capitalizeWord xs == fourTimes capitalizeWord xs
prop_sortIdem xs = sort xs == twice sort xs 
                && sort xs == fourTimes sort xs 

qc_idempotence = do
  quickCheck (prop_capIdem :: String -> Bool)
  quickCheck (prop_sortIdem :: String -> Bool)
  quickCheck (prop_sortIdem :: [Int] -> Bool)

-- GEN RANDOM GENERATOR
data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

eqProb :: Gen Fool 
eqProb = elements [Fulse, Frue]

thirdProb :: Gen Fool 
thirdProb = frequency [ (2, return Fulse)
                      , (1, return Frue) ]








