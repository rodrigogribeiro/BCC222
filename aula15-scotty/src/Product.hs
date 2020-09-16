{-# LANGUAGE DeriveGeneric #-}
module Product where

import Data.Aeson
import Data.IORef
import GHC.Generics
import System.IO.Unsafe -- for hacking global state

data Product
  = Product {
      code :: Int
    , name :: String
    , description :: String
    , price :: Double
    , quantity :: Int
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON Product
instance FromJSON Product

-- XXX this is a hack to avoid using
--     a database

db :: IORef [Product]
db = unsafePerformIO (newIORef database)

insertDB :: Product -> IO ()
insertDB p
  = modifyIORef db (p :)

-- list of products

database :: [Product]
database
  = [
      Product 1 "Pilsen" "Cerveja Pilsen" 20.0 50
    , Product 2 "IPA" "Cerveja IPA" 30.0 20
    , Product 3 "APA" "Cerveja APA" 30.0 30
    , Product 4 "Weiss" "Cerveja Weiss" 30.0 20
    , Product 5 "Stout" "Cerveja Stout" 40.0 10
    ]

-- searching

searchByCode :: Int -> Maybe Product
searchByCode n
  = searchByCode' n database

searchByCode' :: Int -> [Product] -> Maybe Product
searchByCode' n [] = Nothing
searchByCode' n (p : ps)
  | n == code p = Just p
  | otherwise   = searchByCode' n ps
