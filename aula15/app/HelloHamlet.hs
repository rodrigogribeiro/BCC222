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


mainPage :: ActionM ()
mainPage
  =  html $ renderHtml
        [shamlet| <html>
                    <head>
                      <title> Beer Store
                    <body>
                      <h1> Meu Primeiro backend Haskell|]


productTags :: Product -> H.Markup
productTags p
  = [shamlet| <html>
                 <head>
                   <title>Beer Store
                 <body>
                   <h1>#{name p}
                   <p id=desc>#{description p}|]


productPage :: ActionM ()
productPage
  = do
     pcode <- param "code"
     let prd = searchByCode pcode
     case prd of
       Just p  ->
         html $ renderHtml (productTags p)
       Nothing -> errorPage (pack $ show pcode)

errorPage :: Text -> ActionM ()
errorPage s
  = do
      status notFound404
      html $ renderHtml $ H.h1 $ H.toHtml s'
    where
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
