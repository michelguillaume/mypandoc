{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- JSON formatter
-}

module Format.JSON (renderJSON) where

import AST
import Data.List (intercalate)

-- | A tiny JSON AST
data JValue
  = JObject [(String, JValue)]
  | JArray  [JValue]
  | JString String

-- | Render a Document to a JSON string
renderJSON :: Document -> String
renderJSON doc =
  renderValue 0 (JObject
    [ ("header", headerValue (docHeader doc))
    , ("body"  , JArray (map blockValue (docBody doc)))
    ])

-- | Header → JObject
headerValue :: Header -> JValue
headerValue (Header title mAuth mDate) =
  JObject $
     [ ("title", JString title) ]
  ++ maybe [] (\a -> [("author", JString a)]) mAuth
  ++ maybe [] (\d -> [("date",   JString d)]) mDate

-- | Block → JValue
blockValue :: Block -> JValue
blockValue (Paragraph inls) =
  JArray (map inlineValue inls)

blockValue (Section t bs) =
  JObject [("section", JObject
    [ ("title"  , JString t)
    , ("content", JArray (map blockValue bs))
    ])]

blockValue (CodeBlock bs) =
  -- chaque Block devient un élément à l’intérieur de codeblock
  JObject [("codeblock", JArray (map blockValue bs))]

blockValue (Raw s) =
  JString s

blockValue (List items) =
  JObject [("list", JArray (concatMap flatten items))]
  where
    flatten :: [Block] -> [JValue]
    flatten [b]  = [blockValue b]
    flatten blks = map blockValue blks

-- | Inline → JValue
inlineValue :: Inline -> JValue
inlineValue (Plain s) =
  JString s
inlineValue (Bold xs) =
  case xs of
    [Plain s] -> JObject [("bold", JString s)]
    _         -> JObject [("bold", JArray (map inlineValue xs))]
inlineValue (Italic xs) =
  case xs of
    [Plain s] -> JObject [("italic", JString s)]
    _         -> JObject [("italic", JArray (map inlineValue xs))]
inlineValue (CodeSpan s) =
  JObject [("code", JString s)]
inlineValue (Link url xs) =
  JObject [("link", JObject
    [ ("url"    , JString url)
    , ("content", JArray (map inlineValue xs))
    ])]
inlineValue (Image url xs) =
  JObject [("image", JObject
    [ ("url", JString url)
    , ("alt"    , JArray (map inlineValue xs))
    ])]

-- | Render any JValue with indentation
renderValue :: Int -> JValue -> String
renderValue ind (JString s) =
  "\"" ++ escape s ++ "\""
renderValue ind (JArray xs) =
  renderArray ind xs
renderValue ind (JObject kvs) =
  renderObject ind kvs

-- | Render a JSON array with pretty indentation
renderArray :: Int -> [JValue] -> String
renderArray _   [] = "[]"
renderArray ind xs =
  "[\n"
  ++ intercalate ",\n"
       [ spaces (ind+4) ++ renderValue (ind+4) x
       | x <- xs
       ]
  ++ "\n" ++ spaces ind ++ "]"

-- | Render a JSON object with pretty indentation
renderObject :: Int -> [(String,JValue)] -> String
renderObject _   []  = "{}"
renderObject ind kvs =
  "{\n"
  ++ intercalate ",\n"
       [ spaces (ind+4) ++ "\"" ++ escape k ++ "\": " ++ renderValue (ind+4) v
       | (k,v) <- kvs
       ]
  ++ "\n" ++ spaces ind ++ "}"

-- | Escape common JSON characters, including newline and tab
escape :: String -> String
escape = concatMap escapeChar
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar x    = [x]

-- | Utility to indent
spaces :: Int -> String
spaces n = replicate n ' '
