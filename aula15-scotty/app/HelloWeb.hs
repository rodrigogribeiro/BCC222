{-# LANGUAGE OverloadedStrings #-}

module HelloWeb where

import Data.Monoid (mconcat)
import Web.Scotty

main :: IO ()
main
  = scotty 3000 $
      get "/" $
      html $ mconcat [ "<html><body>"
                     , " <h1>Meu Primeiro Backend Haskell!</h1>"
                     , "</body></html>" ]
