module UsingShow where

    data Mood = Blah

    -- defining an instance
    instance Show Mood where
        show _ = "Blah" -- _ is unconditional case

    -- Simply deriving the Show typeclass
    data Mood2 = Blah2 deriving Show
    -- can derive for Eq, Or, Enum, Bounded, Read, Show

