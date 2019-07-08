{-# LANGUAGE FlexibleInstances #-}

module ChapterExercises where

-- making functor instances work 

--1
data Sum a b =
    Second a
  | First b 
  deriving (Eq, Show)

instance Functor (Sum e) where 
  fmap f (First b) = First (f b) 
  fmap f (Second a) = Second a

--2
data Company a b c =
    DeepBlue a b 
  | Something c 
  deriving (Eq, Show)

instance Functor (Company e e') where 
  fmap f (Something c) = Something (f c)
  fmap _ (DeepBlue a b) = DeepBlue a b 

--3
data More a b =
    L b a b 
  | R a b a 
  deriving (Eq, Show)

instance Functor (More x) where 
  fmap f (L b a b') = L (f b) a (f b')
  fmap f (R a b a') = R a (f b) a'

-- writing functor instances

--1
data Quant a b =
    Finance
  | Desk a 
  | Bloor b 
  deriving (Eq, Show)

instance Functor (Quant a) where 
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a

--2
newtype K a b = K a deriving (Eq, Show)

instance Functor (K a) where 
  fmap _ (K a) = K a

--3 
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where 
  fmap f (Flip (K a)) = Flip $ K (f a)

--4 
newtype EvilGoateeConst a b =
  GoatyConst b 
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where 
  fmap f (GoatyConst b) = GoatyConst (f b)

--5
newtype LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where 
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

--6 
data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => 
  Functor (Parappa f g) where 
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

  --7 
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where 
  fmap g (IgnoringSomething fa gb) = 
    IgnoringSomething fa (fmap g gb)

--8
data Notorious g o a t = 
  Notorious (g o) (g a) (g t) 
  deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where 
  fmap g (Notorious go ga gt) = 
    Notorious go ga (fmap g gt) 

--9
data List a =
    Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil  
  fmap f (Cons a as) = Cons (f a) (fmap f as) 

--10
data GoatLord a =
    NoGoat
  | OneGoat a 
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where 
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = 
    MoreGoats (fmap f a) (fmap f b) (fmap f c)

--11
data TalkToMe a = 
    Halt 
  | Print String a 
  | Read (String -> a)

instance Functor TalkToMe where 
  fmap _ Halt = Halt 
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (fmap f g) -- f works on the char in the string




