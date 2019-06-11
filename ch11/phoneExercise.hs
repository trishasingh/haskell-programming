module PhoneExercise where

    import Data.Char
    import Data.Bool
    import Data.List

    -- Data generation
    type DaPhone = [Value]

    -- validButtons = "1234567890*#"
    type Digit = Char
    -- valid presses = [1..4]
    type Presses = Int
    -- value
    type Text = Char

    data Value = 
        Value { digit   :: Digit
              , text    :: Text
              , presses :: Presses }
              deriving (Eq, Show)

    genVal :: Digit -> [Text] -> [Presses] -> [Value]
    genVal txt = zipWith (\a b -> Value txt a b)

    daphone = [Value '1' '1' 1] 
            ++ genVal '2' "abc2"  [1..4]
            ++ genVal '3' "def3"  [1..4]
            ++ genVal '4' "ghi4"  [1..4]
            ++ genVal '5' "jkl5"  [1..4]
            ++ genVal '6' "mno6"  [1..4]
            ++ genVal '7' "pqrs7" [1..5]
            ++ genVal '8' "tuv8"  [1..4]
            ++ genVal '9' "wxyz9" [1..5]
            ++ genVal '0' " +_0"  [1..4]
            ++ genVal '*' "^*"    [1..2]
            ++ genVal '#' ".,#"   [1..3]

    convo :: [String]
    convo = ["Wanna play 20 questions",
             "Ya",
             "U 1st haha",
             "Lol ok. Have u ever tasted alcohol lol",
             "Lol ya",
             "Wow ur cool haha. Ur turn",
             "Ok. Do u think I am pretty Lol",
             "Lol ya",
             "Haha thanks just making sure rofl ur turn"]

    test = "Wanna play 20 questions"

    -- Functions
    cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
    cellPhonesDead daphone message
        | message == [] = []
        | otherwise = (bool [] [('*', 1)] (isUpper x))
                    ++ getTransform getText 
                    ++ cellPhonesDead daphone xs
            where x            = head message
                  xs           = tail message
                  getText      = filter (\a -> text a == toLower x) daphone
                  getTransform = map (\a -> (digit a, presses a))

    transformedConvo = map (cellPhonesDead daphone) convo

    -- How many times do digits need to be pressed for each msg?
    fingerTaps :: [(Digit, Presses)] -> Presses
    fingerTaps = foldr (\a b -> snd a + b) 0

    totalTaps = map fingerTaps transformedConvo

    -- What was the most popular letter in each message?
    popularest :: String -> (Char, Int)
    popularest s = maximumBy compareCounts allCounts
        where allCounts  = map (\a -> (a, elemCount a s)) masterList
              masterList = filter (flip elem allLetters) (nub s)
              allLetters = ['a'..'z'] ++ ['A'..'Z']

    elemCount :: Eq a => a -> [a] -> Int
    elemCount c = (length . filter (==c))

    popLetter = map popularest convo

    -- What was the cost of each message?
    -- How many taps needed to generate a letter
    reverseTaps :: Char -> [(Digit, Presses)]
    reverseTaps c = capitalAdd ++ getTransform
        where getTransform = map (\a -> (digit a, presses a)) getChar
              getChar = filter (\a -> text a == toLower c) daphone
              capitalAdd = bool [] [('*', 1)] (isUpper c)

    popLetterCost :: [(Char, Presses)] -> [Presses]
    popLetterCost xs = map (\a -> nTaps (fst a) * (snd a)) xs
        where nTaps = fingerTaps . reverseTaps

    myPopLetters = popLetterCost popLetter

    -- What was the most popular letter overall?
    coolestLtr :: [String] -> Char
    coolestLtr = fst . (popularest . concat)

    coolestWord :: [String] -> String
    coolestWord xs = fst $ maximumBy compareCounts allCounts
        where allCounts = map (\a -> (a, elemCount a lst)) (nub lst)
              lst = concat $ map words xs

    -- Helper function
    compareCounts :: Ord a1 => (a0, a1) -> (a0, a1) -> Ordering
    compareCounts = \x y -> compare (snd x) (snd y)
    



