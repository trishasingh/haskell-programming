module CurryingEg where

    addStuff a b = a + b + 5

    -- partial application of addStuff
    addTen = addStuff 5

    