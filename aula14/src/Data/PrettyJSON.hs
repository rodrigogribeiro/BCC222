module Data.PrettyJSON (prettyJSON) where

import Data.JSON
import Utils.Pretty

-- rendering JSON values as docs

prettyJSON :: JSON -> Doc
prettyJSON (JString s)
  = string s
prettyJSON (JNumber d)
  = number d
prettyJSON (JBool b)
  = text $ if b then "true"
           else "false"
prettyJSON JNull
  = text "null"
prettyJSON (JObject obj)
  = enclose '{' '}' (nest 3 d)
    where
      d = hcat fields'
      fields' = punctuate (char ',') fields
      fields
        = map build obj
      build (k,v) = (enclose '"' '"' (text k)) <> text " : " <> prettyJSON v
prettyJSON (JArray arr)
  = enclose '[' ']' $ hcat $ punctuate (char ',') $ map prettyJSON arr
