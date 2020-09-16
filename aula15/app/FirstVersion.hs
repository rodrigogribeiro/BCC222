{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}

module FirstVersion where

import Product

import Data.Aeson
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import Servant

-- server API type

type ProductAPI
  =    Get '[JSON] [Product]
  :<|> Capture "productId" Int :> Get '[JSON] Product


productServer :: Server ProductAPI
productServer
  = getProducts :<|> productOperations

getProducts :: Handler [Product]
getProducts
  = return database

productOperations :: Int -> Handler Product
productOperations pid
  = searchByCode pid database
    where
      searchByCode n []
        = throwError (err404 {
            errBody = "No product with the given id exists"})
      searchByCode n (p : ps)
        | n == code p = return p
        | otherwise   = searchByCode n ps


type API = "products" :> ProductAPI

api :: Proxy API
api = Proxy

app :: Application
app = simpleCors $ serve api productServer

main :: IO ()
main = run 8081 app
