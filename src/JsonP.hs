module JsonP
  ( JsonValue(..)
  , jsonValue
  ) where

import Control.Applicative
import Data.Char
import Parser

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNum Int
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Eq)

jsonToString :: Int -> JsonValue -> String
jsonToString indent JsonNull = "null"
jsonToString indent (JsonBool a)
  | a = "true"
  | otherwise = "false"
jsonToString indent (JsonNum x) = show x
jsonToString indent (JsonString s) = show s
jsonToString indent (JsonArray []) = "[]"
jsonToString indent (JsonArray xs) =
  "[" ++
  init (concatMap (showElement (indent + 1)) xs) ++
  "\n" ++ indentation indent ++ "]"
  where
    showElement indent x =
      "\n" ++ indentation indent ++ jsonToString (indent + 1) x ++ ","
jsonToString indent (JsonObject []) = "{}"
jsonToString indent (JsonObject xs) =
  "{" ++ init (concatMap objToString xs) ++ "\n" ++ indentation indent ++ "}"
  where
    objToString (x, y) =
      "\n" ++
      indentation indent ++ show x ++ ": " ++ jsonToString (indent + 1) y ++ ","

indentation :: Int -> String
indentation x = concat $ replicate x "\t"

instance Show JsonValue where
  show = jsonToString 0

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ stringP "true"
    jsonFalse = JsonBool False <$ stringP "false"

jsonNum :: Parser JsonValue
jsonNum = f <$> notNull (spanP isDigit)
  where
    f ds = JsonNum $ read ds

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray =
  JsonArray <$> (charP '[' *> whitespace *> elements <* whitespace <* charP ']')
  where
    elements = sepBy (whitespace *> charP ',' <* whitespace) jsonValue

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject <$>
  (charP '{' *> whitespace *> sepBy (whitespace *> charP ',' <* whitespace) pair <*
   whitespace <*
   charP '}')
  where
    pair =
      (\key _ value -> (key, value)) <$> stringLiteral <*>
      (whitespace *> charP ':' <* whitespace) <*>
      jsonValue

jsonValue :: Parser JsonValue
jsonValue =
  jsonNull <|> jsonBool <|> jsonNum <|> jsonString <|> jsonArray <|> jsonObject
