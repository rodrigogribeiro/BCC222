{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module HelloHamlet where

import Product

import Text.Hamlet

import Data.Text.Lazy
import Data.Monoid (mconcat)
import Network.HTTP.Types
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty


-- root page

mainPage :: ActionM ()
mainPage
  =  html $ renderHtml
        [shamlet| <html>
                    <head>
                      <title> Beer Store
                    <body>
                      <h1> Meu Primeiro backend Haskell|]


--  simple page for showing product information

productTags :: Product -> H.Markup
productTags p
  = [shamlet| <html>
                 <head>
                   <title>Beer Store
                 <body>
                   <h1>#{name p}
                   <p id=desc>#{description p}|]

-- the page for product information

productPage :: ActionM ()
productPage
  = do
     pcode <- param "code"
     let prd = searchByCode pcode
     case prd of
       Just p  ->
         html $ renderHtml (productTags p)
       Nothing -> errorPage (pack $ show pcode)

-- page for error 404

errorPage :: Text -> ActionM ()
errorPage s
  = do
      status notFound404
      html $ renderHtml $ H.h1 $ H.toHtml s'
    where
      s' = if Data.Text.Lazy.null s
           then "Not Found :("
           else "Product not found:" <> s

-- page for listing all products

productListPage :: ActionM ()
productListPage
  = html $ renderHtml
      [shamlet|
         <html>
           <body>
             <h1> Products
             <table>
                <tr>
                  <th> Name
                  <th> Description
                  <th> Price
                $forall ap <- database
                  <tr>
                    <td>#{name ap}
                    <td>#{description ap}
                    <td>#{price ap}|]

-- starting the server


main :: IO ()
main
  = scotty 3000 $
     do
        -- route 1: root application
        get "/" mainPage
        -- route 2: product info page
        get "/product/:code" productPage
        -- route 3: listing products
        get "/products" productListPage
        -- route 4: 404 error page
        notFound $ errorPage ""
