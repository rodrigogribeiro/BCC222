module Data.JSONLib ( module Data.JSON
                    , prettyJSON
                    , parseJSON
                    , ToJSON (..)
                    , FromJSON (..)) where

import Utils.Pretty (pretty)
import Utils.Parser (runParser)
import Data.JSON
import Data.ParserJSON
import qualified Data.PrettyJSON as P


-- interface for the programmer

class ToJSON a where
  toJSON :: a -> JSON

class FromJSON a where
  fromJSON :: JSON -> a


prettyJSON :: JSON -> String
prettyJSON = pretty 100 . P.prettyJSON


parseJSON :: String -> Either String JSON
parseJSON s
  = case runParser jsonParser s of
      [] -> Left "Parse error!"
      ((x,_) : _) -> Right x
