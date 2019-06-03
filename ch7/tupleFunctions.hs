module TupleFunctions where

    addEmUp :: Num a => (a, a) -> a
    addEmUp (x, y) = x + y

    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x

    third3 :: (a, b, c) -> c
    third3 (_, _, x) = x

-- :browse tupleFunctions