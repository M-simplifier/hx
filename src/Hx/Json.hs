module Hx.Json
    ( jsonArray
    , jsonBool
    , jsonMaybe
    , jsonNull
    , jsonNumber
    , jsonObject
    , jsonString
    )
where

import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)

jsonObject :: [(String, String)] -> String
jsonObject fields =
    "{" <> intercalate "," (map renderField fields) <> "}"
  where
    renderField (name, value) = jsonString name <> ":" <> value

jsonArray :: [String] -> String
jsonArray values =
    "[" <> intercalate "," values <> "]"

jsonString :: String -> String
jsonString value =
    "\"" <> concatMap escapeChar value <> "\""

jsonBool :: Bool -> String
jsonBool value =
    if value then "true" else "false"

jsonNumber :: Show a => a -> String
jsonNumber =
    show

jsonNull :: String
jsonNull =
    "null"

jsonMaybe :: (a -> String) -> Maybe a -> String
jsonMaybe renderValue maybeValue =
    case maybeValue of
        Just value -> renderValue value
        Nothing -> jsonNull

escapeChar :: Char -> String
escapeChar char =
    case char of
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\b' -> "\\b"
        '\f' -> "\\f"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ | ord char < 32 -> "\\u" <> leftPad 4 '0' (showHex (ord char) "")
        _ -> [char]

leftPad :: Int -> Char -> String -> String
leftPad width padChar value =
    replicate (max 0 (width - length value)) padChar <> value
