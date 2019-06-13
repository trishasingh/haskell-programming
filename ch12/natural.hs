module Natural where

    data Nat =
        Zero
        | Succ Nat
        deriving (Eq, Show)

    natToInteger :: Nat -> Integer
    natToInteger Zero = 0
    natToInteger (Succ x) = 1 + natToInteger x

    integerToNat :: Integer -> Maybe Nat
    integerToNat x
        | x < 0 = Nothing
        | x >= 0 = Just $ validIntToNat x
    
    validIntToNat :: Integer -> Nat 
    validIntToNat 0 = Zero 
    validIntToNat x = Succ $ validIntToNat (x - 1)