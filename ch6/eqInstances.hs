module EqInstances where

    data DayOfWeek = 
        Mon | Tue | Wed | Thu | Fri | Sat | Sun
        deriving (Ord, Show)

    data Date =
        Date DayOfWeek Int

    instance Eq DayOfWeek where
        (==) Mon Mon = True
        (==) Tue Tue = True
        (==) Wed Wed = True
        (==) Thu Thu = True
        (==) Fri Fri = True
        (==) Sat Sat = True
        (==) Sun Sun = True
        (==) _ _     = True

    instance Eq Date where
        (==) (Date weekday monthNum)
             (Date weekday' monthNum') =
            weekday == weekday' && monthNum == monthNum'

    -- Identity type
    data Identity a =
        Identity a

    instance Eq a => Eq (Identity a) where
        (==) (Identity v) (Identity v') = v == v'

    -- Exercises

    -- 1
    data TisAnInteger = 
        TisAn Integer 

    instance Eq TisAnInteger where
        (==) (TisAn v) (TisAn v') = v == v'

    -- 2
    data TwoIntegers = 
        Two Integer Integer

    instance Eq TwoIntegers where
        (==) (Two int1 int2) 
             (Two int1' int2')
            = int1 == int1' && int2 == int2'

    -- 3
    data StringOrInt =
        TisAnInt Int
        | TisAString String

    instance Eq StringOrInt where
        (==) (TisAnInt v) (TisAnInt v')     = v == v'
        (==) (TisAString v) (TisAString v') = v == v'
        (==) _ _                            = False

    -- 4
    data Pair a =
        Pair a a 
    
    instance Eq a => Eq (Pair a) where
        (==) (Pair x y)
             (Pair x' y') 
            = x == x' && y == y'

    -- 5
    data Tuple a b =
        Tuple a b
    
    instance (Eq a, Eq b) => Eq (Tuple a b) where
        (==) (Tuple x y)
             (Tuple x' y')
            = x == x' && y == y'

    -- 6
    data Which a =
        ThisOne a 
        | ThatOne a

    instance Eq a => Eq (Which a) where
        (==) (ThisOne v) (ThisOne v') = v == v'
        (==) (ThatOne v) (ThatOne v') = v == v'
        (==) _ _                      = False

    -- 7
    data EitherOr a b =
        Hello a
        | GoodBye b 

    instance (Eq a, Eq b) => Eq (EitherOr a b) where
        (==) (Hello v) (Hello v')     = v == v'
        (==) (GoodBye v) (GoodBye v') = v == v'
        (==) _ _                      = False

-- :set -Wall         -- To check for non exhaustive cases








 