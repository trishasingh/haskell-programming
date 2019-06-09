module Jammin where

    import Data.List

    data Fruit =
        Peach
        | Plum
        | Apple
        | Blackberry
        deriving (Eq, Ord, Show)

    data JamJars =
        JamJars { fruit   :: Fruit
                , jars :: Int }
        deriving (Eq, Ord, Show)

    -- sample data
    row1 = JamJars Peach 5
    row2 = JamJars Plum 20
    row3 = JamJars Apple 55
    row4 = JamJars Blackberry 0
    row5 = JamJars Peach 10
    row6 = JamJars Apple 2
    allJam = [row1, row2, row3, row4, row5, row6]

    totalJars :: [JamJars] -> Int
    totalJars = sum . map jars
    
    mostRow :: [JamJars] -> JamJars
    mostRow = maximumBy (\a b -> compare (jars a) (jars b))
    
    sortJam :: [JamJars] -> [JamJars]
    sortJam = sortBy compareKind
        where compareKind (JamJars a _) (JamJars b _) =
                compare a b

    sortedJam = sortJam allJam

    groupJam :: [JamJars] -> [[JamJars]]
    groupJam jams = groupBy compareKind $ sortJam jams
            where compareKind (JamJars a _) (JamJars b _) = 
                    a == b
 
    


    