module Vehicles where

    import Data.Maybe

    data Price =
        Price Integer deriving (Eq, Show)

    data Size = 
        Size Double deriving (Eq, Show)

    data Manufacturer =
            Mini
          | Mazda
          | Tata
          deriving (Eq, Show)

    data Airline =
            PapuAir
          | CatapultsR'Us
          | TakeYourChancesUnited
          deriving (Eq, Show)

    data Vehicle =
            Car Manufacturer Price
          | Plane Airline Size
          deriving (Eq, Show)

    -- sample data
    myCar = Car Mini (Price 14000)
    urCar = Car Mazda (Price 20000)
    clownCar = Car Tata (Price 7000)
    doge = Plane PapuAir (Size 3452)

    -- functions
    isCar :: Vehicle -> Bool
    isCar v = case v of
        Car _ _ -> True 
        _       -> False

    isPlane :: Vehicle -> Bool
    isPlane (Plane _ _) = True
    isPlane _           = False

    areCars :: [Vehicle] -> [Bool]
    areCars = map isCar

    getManu :: Vehicle -> Maybe Manufacturer
    getManu v = case v of
        Car manu _ -> Just manu
        _          -> Nothing

    


        