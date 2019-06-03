module ChangeMood where

    data Mood = Blah | Woot deriving Show
    
    changeMood :: Mood -> Mood
    changeMood Blah = Woot
    changeMood Woot = Blah

    -- Mood is type constructor
    -- Blah and Woot at data constructors