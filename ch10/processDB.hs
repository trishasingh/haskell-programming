module ProcessDB where

    -- Setup
    import Data.Time
    import Data.Maybe

    data DatabaseItem = 
        DbString String 
      | DbNumber Integer
      | DbDate   UTCTime
      deriving (Eq, Ord, Show)

    theDatabase :: [DatabaseItem]
    theDatabase =
        [ DbDate (UTCTime
                 (fromGregorian 1911 5 1)
                 (secondsToDiffTime 34123))
        , DbString "Hello, world!"
        , DbNumber 3435
        , DbDate (UTCTime
                 (fromGregorian 1921 5 1)
                 (secondsToDiffTime 34123))
        , DbNumber 2001
        ]


    -- Different ways to filter out date values
    filterDbDate :: [DatabaseItem] -> [UTCTime]
    filterDbDate = foldr (\a b -> filterHelper a ++ b) []

    filterHelper :: DatabaseItem -> [UTCTime]
    filterHelper (DbDate date) = [date]
    filterHelper _             = []

    filterDbDate' :: [DatabaseItem] -> [UTCTime]
    filterDbDate' = foldr go []
        where go a b = case a of 
                (DbDate date) -> date : b
                _             -> b

    -- avoiding recursively concatenating lists
    filterDbDate'' :: [DatabaseItem] -> [UTCTime]
    filterDbDate'' db = concat xs
        where xs = fmap filterHelper db
    
    -- Using maybe
    filterDbDate''' :: (DatabaseItem -> Maybe UTCTime) -> [DatabaseItem] -> [UTCTime]
    filterDbDate''' getter db = catMaybes xs
            where xs = fmap getter db 
    -- general format for getter
    getDate :: DatabaseItem -> Maybe UTCTime
    getDate (DbDate date) = Just date 
    getDate _             = Nothing


    -- Filtering out DbNumber values

    -- avec fold
    filterDbNum :: [DatabaseItem] -> [Integer]
    filterDbNum = foldr go []
            where go a b = 
                    case a of
                        (DbNumber n) -> n : b
                        _            -> b

    -- sans fold
    filterDbNum' :: [DatabaseItem] -> [Integer]
    filterDbNum' db = catMaybes xs
        where xs = fmap getNum db 
    
    getNum :: DatabaseItem -> Maybe Integer 
    getNum (DbNumber n) = Just n 
    getNum _            = Nothing


    -- Most recent date
    mostRecent :: [DatabaseItem] -> UTCTime
    mostRecent = maximum . filterDbDate 


    -- Sum DbNumber values
    sumDb :: [DatabaseItem] -> Integer
    sumDb = sum . filterDbNum


    -- Avg of DbNumber values
    avgDb :: [DatabaseItem] -> Double
    avgDb db = fromIntegral sumNums / fromIntegral lenNums
        where nums     = filterDbNum db
              sumNums  = sum nums
              lenNums  = length nums
