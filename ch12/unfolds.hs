module Unfolds where

    -- Tree datatype
    data BinaryTree a =
        Leaf
        | Node (BinaryTree a) a (BinaryTree a)
        deriving (Eq, Ord, Show)

    myIterate :: (a -> a) -> a -> [a]
    myIterate f x = x : myIterate f (f x)

    myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    myUnfoldr f x = case f x of 
        Nothing -> []
        (Just (a, b)) -> a : myUnfoldr f b

    betterIterate :: (a -> a) -> a -> [a]
    betterIterate f x =
        myUnfoldr (\a -> Just (a, f a)) x

    unfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
    unfoldTree f a = case f a of
        Nothing -> Leaf
        (Just (x,y,z)) -> Node (unfoldTree f x) y 
                               (unfoldTree f z)

    treeBuild :: Integer -> BinaryTree Integer
    treeBuild i = unfoldTree f 0 
        where f x
                | x == i = Nothing 
                | otherwise = Just (x+1, x, x+1)

    