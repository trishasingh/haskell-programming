module EitherLib where

    import Data.Bool

    lefts' :: [Either a b] -> [a]
    lefts' = foldr lefts'' []
        where lefts'' = \a b -> bool [] [getVal a] (isLeft a) 
                             ++ b
              getVal (Left a) = a

    rights' :: [Either a b] -> [b]
    rights' = foldr rights'' []
        where rights'' = \a b -> bool [] [getVal a] (isRight a) 
                              ++ b
              getVal (Right a) = a

    partitionEithers' :: [Either a b] -> ([a], [b])
    partitionEithers' xs = (lefts' xs, rights' xs)

    eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
    eitherMaybe' f (Right x)  = Just $ f x 
    eitherMaybe' _ _          = Nothing

    either' :: (a -> c) -> (b -> c) -> Either a b -> c
    either' f _ (Left x)  = f x
    either' _ f (Right x) = f x

    eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
    eitherMaybe'' f x = either' (\a -> Nothing) 
                        (\b -> Just (f b)) x


     
                   


    -- helper funcs
    isLeft :: Either a b -> Bool
    isLeft (Left _)  = True
    isLeft (Right _) = False

    isRight :: Either a b -> Bool
    isRight = not . isLeft

