module Dogs where

    data PugType = PugData -- type constant, data constant
 
    data HuskyType a = HuskyData -- type constructor, data constant

    data DogueDeBordeaux doge = DogueDeBordeaux doge -- type constructor, data constructor

    myPug = PugData :: PugType

    myHusky :: HuskyType a
    myHusky = HuskyData

    myOtherHusky :: Num a => HuskyType a
    myOtherHusky = HuskyData

    myDoge :: DogueDeBordeaux Int
    myDoge = DogueDeBordeaux 10

    data Doggies a =
        Husky a
      | Mastiff a
      deriving (Eq, Show)
    -- :k Doggies
    -- :t Husky