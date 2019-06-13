module MaybeLib where

    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust Nothing  = False

    isNothing :: Maybe a -> Bool
    isNothing = not . isJust

    mayybee :: b -> (a -> b) -> Maybe a -> b
    mayybee x _ Nothing  = x
    mayybee _ f (Just x) = f x

    fromMaybe :: a -> Maybe a -> a
    fromMaybe x Nothing  = x
    fromMaybe _ (Just x) = x

    fromMaybe' :: a -> Maybe a -> a
    fromMaybe' x = mayybee x id

    listToMaybe :: [a] -> Maybe a
    listToMaybe []    = Nothing
    listToMaybe (x:_) = Just x

    maybeToList :: Maybe a -> [a]
    maybeToList Nothing  = []
    maybeToList (Just x) = [x] 

    catMaybes :: [Maybe a] -> [a]
    catMaybes xs = concat $ map f xs
        where f x = case x of 
                        Nothing -> []
                        (Just a) -> [a] 

    -- flipMaybe :: [Maybe a] -> Maybe [a]
    flipMaybe xs = case allJust of 
        True -> Just $ map getVal xs
        False -> Nothing 
        where allJust = foldr (\a b -> isJust a && b) 
                        True xs
              getVal (Just a) = a
