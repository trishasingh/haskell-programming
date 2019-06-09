{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module LivestockCounts where

    newtype Goats = Goats Int deriving (Eq, Show)
    newtype Cows  = Cows Int deriving (Eq, Show, TooMany)

    -- data Livestock = Goats Int 
    --                | Cows Int
    --                deriving (Eq, Show)

    tooManyGoats :: Goats -> Bool
    tooManyGoats (Goats n) = n > 42

    tooManyCows :: Cows -> Bool
    tooManyCows (Cows n)  = n > 20

    -- defining instances

    class TooMany a where
        tooMany :: a -> Bool 

    instance TooMany Int where 
        tooMany n = n > 42 

    instance TooMany Goats where 
        tooMany (Goats n) = n > 43

    -- TooMany has been derived for Cows, behaves like Int

    -- Exercises
    instance TooMany String where 
        tooMany s = length s > 10

    instance TooMany (Int, String) where 
        tooMany (i, s) = (tooMany i) && (tooMany s)

    instance (Num a, TooMany a) => TooMany (a, a) where 
        tooMany (a1, a2) = tooMany (a1 + a2)
