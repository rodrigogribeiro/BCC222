{-# LANGUAGE OverloadedStrings #-}

module HelloBlaze where

import Product

import Data.Text.Lazy
import Data.Monoid (mconcat)
import Network.HTTP.Types
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty


mainPage :: ActionM ()
mainPage
  =  html $ renderHtml $
       do
         H.head (H.title "Beer Store")
         H.body (H.h1 t)
     where
       t = "Meu Primeiro backend Haskell"


productTags :: Product -> H.Markup
productTags p
  = do
      H.head (H.title "Beer Store")
      H.body $ do
        H.h1 $ H.toHtml (pack (name p))
        H.p H.! A.id "descr" $ H.toHtml (pack (description p))
        

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

