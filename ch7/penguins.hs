module Penguins where

    -- sum type
    data WherePenguinsLive =
        Galapagos
      | Antarctic
      | Australia
      | SouthAfrica
      | SouthAmerica
      deriving (Eq, Show)

    -- product type
    data Penguin =
        Peng WherePenguinsLive
        deriving (Eq, Show)

    isSouthAfrica :: WherePenguinsLive -> Bool
    isSouthAfrica SouthAfrica = True
    isSouthAfrica _           = False

    gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
    gimmeWhereTheyLive (Peng whereitlives) = whereitlives

    antarcticPenguin :: Penguin -> Bool
    antarcticPenguin (Peng Antarctic) = True
    antarcticPenguin _                = False

    galapagosPenguin :: Penguin -> Bool
    galapagosPenguin (Peng Galapagos) = True
    galapagosPenguin _                = False

    antarcticOrGalapagos :: Penguin -> Bool
    antarcticOrGalapagos p = 
        antarcticPenguin p || galapagosPenguin p

-- Test penguins: 
-- humboldt = Peng SouthAmerica
-- gentoo = Peng Antarctic
-- macaroni = Peng Antarctic
-- little = Peng Australia
-- galapagos = Peng Galapagos
