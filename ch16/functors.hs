{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Functors where

import Test.QuickCheck

{-# ANN module "Hlint: ignore" #-}

data FixMePls a = 
    FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where 
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

-- partially applied types to make functor instance
data Two a b = 
  Two a b
  deriving (Eq, Show)

data Or a b = 
    First a 
  | Second b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap _ (First a) = First a 
  fmap f (Second b) = Second (f b) 

-- quickchecking functor instances
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == id f

functorCompose :: (Eq (f c), Functor f) => 
  (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = 
  fmap (g . f) x == (fmap g . fmap f) x

-- quickcheck generates functions
functorCompose' :: (Eq (f c), Functor f) =>
  f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  fmap (g . f) x == (fmap g . fmap f) x

type IntToInt = Fun Int Int 
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

qc_functorLaws = do
  quickCheck (\x -> functorIdentity (x :: [Int]))
  quickCheck (\x -> functorCompose (+1) (+2) (x :: [Int]))
  quickCheck (functorCompose' :: IntFC)

-- ignoring possiblities: maybe
incIfJust :: Num a => Maybe a -> Maybe a 
incIfJust (Just n) = Just $ n + 1 
incIfJust Nothing  = Nothing 

showIfJust :: Show a => Maybe a -> Maybe String 
showIfJust (Just s) = Just $ show s 
showIfJust Nothing  = Nothing 

incMaybe :: Num a => Maybe a -> Maybe a 
incMaybe = fmap (+1)

showMaybe :: Show a => Maybe a -> Maybe String 
showMaybe = fmap show

liftedInc :: (Num a, Functor f) => f a -> f a 
liftedInc = fmap (+1)

liftedShow :: (Show a, Functor f) => f a -> f String
liftedShow = fmap show

-- short exercise: write maybe functor instance
data Possibly a =
    LolNope
  | Yup a 
  deriving (Eq, Show)

instance Functor Possibly where 
  fmap f (Yup a) = Yup (f a)
  fmap f LolNope = LolNope

-- ignoring possibilities: either
incIfRight :: Num a => Either e a -> Either e a 
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e)  = Left e

showIfRight :: Show a => Either e a -> Either e String 
showIfRight (Right s) = Right $ show s 
showIfRight (Left e)  = Left e 

incRight :: Num a => Either e a -> Either e a 
incRight = fmap (+1)

showRight :: Show a => Either e a -> Either e String 
showRight = fmap show 

-- can also use prev lifted functions

-- short exercise: write either functor instance 
data Sum a b =
    First' a 
  | Second' b 
  deriving (Eq, Show)

instance Functor (Sum a) where 
  fmap _ (First' a) = First' a
  fmap f (Second' b) = Second' (f b)

-- constant datatype
data Constant a b = 
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant m) where 
  fmap _ (Constant v) = Constant v 

-- more structure, more functions
data Wrap f a = 
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where 
  fmap f (Wrap x) = Wrap (fmap f x)

-- IO
getInt :: IO Int 
getInt = fmap read getLine

bumpIt :: IO Int 
bumpIt = do 
  intVal <- getInt 
  return (intVal + 1)
-- instead: fmap (+1) getInt

-- changing structure not contents
type Nat f g = forall a . f a -> g a
-- in repl: :set -XRank2Types

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

type BadNat f g a = forall a . f a -> g a 

badMaybeToList :: BadNat Maybe [] a 
badMaybeToList Nothing = [] 
badMaybeToList (Just a) = [a]

-- unique functor instances only
data Tuple a b =
  Tuple a b 
  deriving (Eq, Show)

newtype Flip f a b = 
  Flip (f b a) 
  deriving (Eq, Show)

instance Functor (Flip (Tuple a)) where 
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b 

