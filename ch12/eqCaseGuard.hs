module EqCaseGuard where

    data PersonInvalid' = NameEmpty'
                        | AgeTooLow'

    -- Compiles fine without Eq
    toString :: PersonInvalid' -> String
    toString NameEmpty' = "NameEmpty"
    toString AgeTooLow' = "AgeTooLow"
    
    instance Show PersonInvalid' where
        show = toString

    -- Writing an Eq instance
    instance Eq PersonInvalid' where
        (==) NameEmpty' NameEmpty' = True
        (==) AgeTooLow' AgeTooLow' = True
        (==) _ _                 = False

    -- Testing the Eq instance
    blah :: PersonInvalid' -> String
    blah pi
        | pi == NameEmpty' = "NameEmpty"
        | pi == AgeTooLow' = "AgeTooLow"
        | otherwise = "???"

    -- Using Either to makeperson
    type Name = String
    type Age = Integer

    data Person = Person Name Age deriving Show
    data PersonInvalid = NameEmpty
                       | AgeTooLow
                       deriving (Eq, Show)
    
    mkPerson :: Name 
             -> Age 
             -> Either PersonInvalid Person

    mkPerson name age 
        | name /= "" && age >= 0 = Right $ Person name age
        | name == ""             = Left NameEmpty
        | otherwise              = Left AgeTooLow

    -- List of invalid person

    type ValidatePerson a = Either [PersonInvalid] a

    ageOkay :: Age -> Either [PersonInvalid] Age
    ageOkay age = case age >= 0 of
        True -> Right age
        False -> Left [AgeTooLow]

    nameOkay :: Name -> Either [PersonInvalid] Name
    nameOkay name = case name /= "" of
        True -> Right name
        False -> Left [NameEmpty]

    mkPerson' :: Name -> Age -> ValidatePerson Person 
    mkPerson' name age = mkPerson'' (nameOkay name) (ageOkay age)

    mkPerson'' :: ValidatePerson Name
               -> ValidatePerson Age
               -> ValidatePerson Person 

    mkPerson'' (Right nameOk) (Right ageOk) =
        Right $ Person nameOk ageOk 
    mkPerson'' (Left badName) (Left badAge) =
        Left $ badName ++ badAge 
    mkPerson'' (Left badName) _ = Left badName 
    mkPerson'' _ (Left badAge)  = Left badAge





    

