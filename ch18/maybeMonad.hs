module MaybeMonad where

{- HLINT ignore "Use <$> -}

data Cow = Cow {
    name :: String
  , age :: Int 
  , weight :: Int 
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String 
noEmpty "" = Nothing 
noEmpty str = Just str 

noNegative :: Int -> Maybe Int 
noNegative n | n >= 0 = Just n
             | otherwise = Nothing 

-- if cow's name is Bess, weight must be less than 500 
weightCheck :: Cow -> Maybe Cow 
weightCheck c = 
  let w = weight c 
      n = name c 
  in if n == "Bess" && w > 499 
     then Nothing
     else Just c 

mkSphericalCow :: String -> Int -> Int -> Maybe Cow 
mkSphericalCow name' age' weight' =
  case noEmpty name' of 
    Nothing -> Nothing 
    Just nammy -> 
      case noNegative age' of 
        Nothing -> Nothing
        Just agey -> 
          case noNegative weight' of 
            Nothing -> Nothing
            Just weighty -> 
              weightCheck (Cow nammy agey weighty)

-- clean it up with monad 
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow 
mkSphericalCow' name' age' weight' = do 
  nammy <- noEmpty name' 
  agey <- noNegative age' 
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

-- try with bind 
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow 
mkSphericalCow'' name' age' weight' = 
  noEmpty name' >>= 
    \nammy -> noNegative age' >>=
      \agey -> noNegative weight' >>=
        \weighty ->
          weightCheck (Cow nammy agey weighty)

-- why not applicative
f :: Maybe Integer 
f = Just 1 
g :: Maybe String 
g = Just "1"
h :: Maybe Integer
h = Just 10191

zed :: a -> b -> c -> (a, b, c)
zed = (,,)

doSomething = do 
  a <- f 
  b <- g 
  c <- h 
  return (zed a b c)

doSomethingA = 
  zed <$> f <*> g <*> h

zed' :: Monad m => a -> b -> c -> m (a, b, c)
zed' a b c = return (a, b, c)

doSomething' = do
  a <- f 
  b <- g
  c <- h 
  zed' a b c
  




