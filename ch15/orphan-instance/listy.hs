module Listy where

import Data.Monoid 

newtype Listy a =
  Listy [a]
  deriving (Eq, Show)
