module Data.JSONLib ( module Data.JSON
                    , prettyJSON
                    , parseJSON) where

import Utils.Parser (runParser)
import Data.JSON
import Data.ParserJSON
import Data.PrettyJSON


parseJSON :: String -> Either String JSON
parseJSON s
  = case runParser jsonParser s of
      [] -> Left "Parse error!"
      ((x,_) : _) -> Right x
