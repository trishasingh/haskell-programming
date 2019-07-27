module ValidatePerson where 

import Control.Applicative

validateLength :: Int -> String -> Maybe String 
validateLength maxLen s = 
  if length s > maxLen 
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name  
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address 
mkAddress s = Address <$> validateLength 100 s

-- smart constructor for person
data Person = 
  Person Name Address 
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person 
mkPerson n a = Person <$> mkName n <*> mkAddress a

-- cow example
data Cow = Cow {
    name   :: String
  , age    :: Int 
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String 
noEmpty ""  = Nothing 
noEmpty str = Just str 

noNegative :: Int -> Maybe Int 
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow 
cowFromString n a w = 
  Cow <$> noEmpty n 
      <*> noNegative a 
      <*> noNegative w

cowFromString' :: String -> Int -> Int -> Maybe Cow 
cowFromString' n a w= liftA3 Cow (noEmpty n)
                                 (noNegative a)
                                 (noNegative w)



