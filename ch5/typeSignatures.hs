module TypeSignatures where

    -- destructuring a single element of a list
    functionH :: [a] -> a
    functionH (x:_) = x

    functionC :: Ord a => a -> a -> Bool
    functionC x y = if (x > y) then True else False

    functionS :: (a, b) -> b
    functionS (x, y) = y

    -- given a type write the function
    i :: a -> a
    i x = x

    c :: a -> b -> a
    c x y = x

    c' :: a -> b -> b 
    c' x y = y

    r :: [a] -> [a]
    r x = reverse x

    co :: (b -> c) -> (a -> b) -> (a -> c)
    f :: b -> c; f = undefined
    g :: a -> b; g = undefined
    co f g x = f (g x)

    a :: (a -> c) -> a -> a
    a f x = x

    a' :: (a -> b) -> a -> b
    a' f x = f x
