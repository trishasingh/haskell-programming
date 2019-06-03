module BindExp where

    bindExp :: Integer -> String
    bindExp x = let y = 5 in -- can't switch order
                let z = y + x in "the integer was: "
                ++ show x ++ " and y was: "
                ++ show y ++ " and z was: "
                ++ show z

    shadowExp :: Integer -> String
    shadowExp x = let x = 10; y = 5 in
                  "the integer was: " ++ show x
                  ++ " and y was: " ++ show y
    -- value of x doesn't change w arg because x = 10 
    -- shadows it; lexical scoping
    
