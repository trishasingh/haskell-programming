module Spines where

    num :: [Int]; num = [1,2,3]
    num' :: [Int]; num' = [1..3]
    -- :sprint num -- fully evaluated NF
    -- :sprint num' -- WHNF

    -- recurive length function; forces spine
    length' :: [a] -> Integer
    length' [] = 0
    length' (_:xs) = 1 + length' xs

    -- this will throw error bc spine is undefined
    -- x = [1] ++ undefined ++ [2]

    -- sum function; forces spine and values
    sum' :: Num a => [a] -> a 
    sum' [] = 0
    sum' (x : xs) = x + sum' xs



