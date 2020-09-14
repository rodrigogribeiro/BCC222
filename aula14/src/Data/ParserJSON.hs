module Data.ParserJSON (jsonParser) where

import Control.Applicative (liftA2)
import Data.JSON
import Utils.Parser

-- top level parsing function

jsonParser :: Parser Char JSON
jsonParser
  = nullParser   <|>
    boolParser   <|>
    stringParser <|>
    numberParser <|>
    arrayParser  <|>
    objectParser  
    
-- utility to remove spaces

lexer :: String -> Parser Char String
lexer s
  = f <$> spaces <*> token s
   where
     f _ x = x

-- basic parsers

nullParser :: Parser Char JSON
nullParser = JNull <$ lexer "null"

boolParser :: Parser Char JSON
boolParser
  = trueParser <|> falseParser
    where
      trueParser
        = JBool True <$ lexer "true"
      falseParser
        = JBool False <$ lexer "false"

numberParser :: Parser Char JSON
numberParser
  = JNumber <$> natural

stringLiteral :: Parser Char String
stringLiteral
  = pack open p close
    where
      p = many (sat normal)
      open = symbol '"'
      close = symbol '"'
      normal c = c `notElem` "\""

stringParser :: Parser Char JSON
stringParser
  = JString <$> stringLiteral

-- array and object parser

arrayParser :: Parser Char JSON
arrayParser
  = JArray <$> (ob *> spaces
                   *> elements
                   <* spaces
                   <* cb)
    where
      ob = symbol '['
      cb = symbol ']'
      elements = listOf jsonParser comma
      comma = spaces *> symbol ',' <* spaces

objectParser :: Parser Char JSON
objectParser
  = JObject <$> (ob *> spaces
                    *> listOf pair comma
                    <* spaces
                    <* cb)
    where
      ob = symbol '{'
      cb = symbol '}'
      comma = spaces *> symbol ',' <* spaces
      pair = liftA2 (,) field jsonParser
      field = stringLiteral <* spaces
                            <* symbol ':'
                            <* spaces
