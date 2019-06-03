module IntegralDivision where
    
    -- function to perform division
    dividedBy :: Integer -> Integer -> (DividedResult, Maybe Integer)
    dividedBy num denom = go num denom 0
        where go n d count
                | d == 0 = (DividedByZero, Nothing)
                | abs n < abs d = (Result count, Just n)
                | otherwise = go (n - nSub) d (count + countAdd)
                    where nSub     = if n >= 0 
                                        then abs d 
                                        else negate (abs d)
                          countAdd = if (n * d) >= 0 
                                        then 1 
                                        else negate 1

    -- define type to handle 0 vals    
    data DividedResult = 
        Result Integer
      | DividedByZero
      deriving Show
                




    -- ALT TYPE SIGNATURES:
    -- type Numerator = Integer
    -- type Denominator = Integer
    -- type Quotient = Integer
    -- dividedBy :: Numerator -> Denominator -> Quotient
    -- dividedBy = div

