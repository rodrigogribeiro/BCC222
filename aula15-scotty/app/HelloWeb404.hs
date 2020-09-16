{-# LANGUAGE OverloadedStrings #-}

module HelloWeb404 where

import Data.Monoid (mconcat)
import Web.Scotty
import Network.HTTP.Types


-- mainPage :: [Text]
mainPage
  =  [ "<html><body>"
     , " <h1>Meu Primeiro Backend Haskell!</h1>"
     , "</body></html>" ]

main :: IO ()
main
  = scotty 3000 $
     do
        -- route 1: root application
        get "/" $ html $ mconcat mainPage
        -- route 2: 404 error page
        notFound $ do
          status notFound404
          html "<h1>Not found :(</h1>"
