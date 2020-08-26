{-# LANGUAGE OverloadedLabels #-}

module Model where

-- name and surname

newtype Name 
  = Name String
    deriving (Eq, Ord, Show)

newtype Surname
  = Surname String
    deriving (Eq, Ord, Show)

data Person
  = Person Name Surname
    deriving (Eq, Ord, Show)

-- Address information

newtype Street
  = Street String
    deriving (Eq, Ord, Show)

newtype City
  = City String
    deriving (Eq, Ord, Show)

newtype District
  = District String
    deriving (Eq, Ord, Show)

newtype Province
  = Province String
    deriving (Eq, Ord, Show)

newtype Country
  = Country String
    deriving (Eq, Ord, Show)

data Address
  = Address Street
            District
            City
            Province
            Country
     deriving (Eq, Ord, Show)

data Client i
  = Company i Name Person Address
  | Individual i Person Address
  deriving (Eq, Ord, Show)

