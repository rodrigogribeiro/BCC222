{-# LANGUAGE DeriveGeneric #-}

module Product where

import Data.Aeson
import GHC.Generics

data Product
  = Product {
      code        :: Int
    , name        :: String
    , description :: String
    , price       :: Float
    , quantity    :: Int
    } deriving (Eq, Ord, Show, Generic)

-- JSON stuff

instance ToJSON Product
instance FromJSON Product


-- simple product list

database :: [Product]
database
  = [
      Product 1 "Pilsen" "Cerveja Pilsen" 20.0 50
    , Product 2 "IPA" "Cerveja IPA" 30.0 20
    , Product 3 "APA" "Cerveja APA" 30.0 30
    , Product 4 "Weiss" "Cerveja Weiss" 30.0 20
    , Product 5 "Stout" "Cerveja Stout" 40.0 10
    ]
