module StoreModel where

-- Simple module to represent
-- our store data.

newtype Name
    = Name {unName :: String}
      deriving (Eq, Ord, Show)

newtype BeerType
    = BeerType {unBeer :: String}
      deriving (Eq, Ord, Show)

data Product
   = Product {
       name  :: Name
     , kind  :: BeerType
     , price :: Double
     } deriving (Eq, Ord, Show)


-- sample data

database :: [Product]
database = [ Product (Name "Kaiser")
                     (BeerType "Larger")
                     2.0
           , Product (Name "Baden")
                     (BeerType "Weiss")
                     10.0
           , Product (Name "Petroleum")
                     (BeerType "Stout")
                     20.0
           , Product (Name "Colorado Indica")
                     (BeerType "IPA")
                     20.0]
