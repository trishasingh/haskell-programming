module EitherMonad where 

-- years ago
type Founded = Int
-- number of programmers 
type Coders = Int 

data SoftwareShop =
  Shop {
      founded :: Founded
    , coders :: Coders
  } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded 
  | NegativeCoders Coders 
  | TooManyCoders Coders 
  | TooManyCodersForYears Founded Coders 
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded 
validateFounded n 
  | n < 0 = Left $ NegativeYears n 
  | n > 500 = Left $ TooManyYears n 
  | otherwise = Right n 

validateCoders :: Int -> Either FoundedError Coders 
validateCoders n 
  | n < 0 = Left $ NegativeCoders n 
  | n > 5000 = Left $ TooManyCoders n 
  | otherwise = Right n 

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware founded coders = do 
  founded' <- validateFounded founded
  coders' <- validateCoders coders 
  if coders' > div founded' 10 
    then Left $ TooManyCodersForYears founded' coders'
    else Right $ Shop founded' coders'

-- implementing the either monad
data Sum a b = 
    First a 
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where 
  fmap _ (First a) = First a 
  fmap f (Second b) = Second $ f b 

instance Applicative (Sum a) where 
  pure = Second
  (First a) <*> _ = First a 
  _ <*> (First a) = First a 
  (Second f) <*> (Second b) = Second $ f b

instance Monad (Sum a) where 
  return = pure 
  First a >>= _ = First a
  Second b >>= f = f b

