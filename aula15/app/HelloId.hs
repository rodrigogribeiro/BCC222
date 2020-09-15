{-# LANGUAGE OverloadedStrings #-}

module HelloId where

import Product

import Data.Text.Lazy
import Data.Monoid (mconcat)
import Web.Scotty
import Network.HTTP.Types


mainPage :: ActionM ()
mainPage
  =  html $ mconcat
        [ "<html><body>"
        ,  "<h1>Meu Primeiro Backend Haskell!</h1>"
        , "</body></html>" ]

productPage :: ActionM ()
productPage
  = do
     pcode <- param "code"
     let prd = searchByCode pcode
     case prd of
       Just p  ->
         html $ mconcat
                  [
                    "<html><body>"
                  , "<h1>"
                  , pack (name p)
                  , "</h1>"
                  , "<p>"
                  , pack (description p)
                  , "</p>"
                  , "</body></html>"
                  ]
       Nothing -> errorPage (pack $ show pcode)

errorPage :: Text -> ActionM ()
errorPage s
  = do
      status notFound404
      html str
    where
      str = "<h1>" <> s' <> "</h1>"
      s' = if Data.Text.Lazy.null s
           then "Not Found :("
           else "Product not found:" <> s


main :: IO ()
main
  = scotty 3000 $
     do
        -- route 1: root application
        get "/" mainPage
        -- route 2: product info page
        get "/product/:code" productPage
        -- route 3: 404 error page
        notFound $ errorPage ""
