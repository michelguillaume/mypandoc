{-# LANGUAGE LambdaCase #-}
module Format.JSON (renderJSON) where

import AST
import Data.List (intercalate)

data JValue
  = JObject [(String, JValue)]
  | JArray  [JValue]
  | JString String

renderJSON :: Document -> String
renderJSON doc =
  renderValue 0 $
    JObject
      [ ("header", headerValue (docHeader doc))
      , ("body"  , JArray  (map blockValue (docBody doc)))
      ]

headerValue :: Header -> JValue
headerValue (Header title mAuth mDate) =
  JObject $
    [ ("title", JString title) ]
    ++ maybe [] (\a -> [("author", JString a)]) mAuth
    ++ maybe [] (\d -> [("date"  , JString d)]) mDate

blockValue :: Block -> JValue
blockValue = \case
  Paragraph inls ->
    JArray (map inlineValue inls)

  CodeBlock codes ->
    JObject [("codeblock", JArray (map JString codes))]

  List items ->
    JObject
      [ ("list", JArray
          [ JArray (map (JString . inlineText) inls)
          | inls <- items
          ])
      ]

  Section title blocks ->
    JObject
      [ ("section", JObject
          [ ("title"  , JString title)
          , ("content", JArray (map blockValue blocks))
          ])
      ]

inlineValue :: Inline -> JValue
inlineValue = \case
  Plain s ->
    JString s

  Bold xs ->
    JObject [("bold",   JString (concatMap inlineText xs))]

  Italic xs ->
    JObject [("italic", JString (concatMap inlineText xs))]

  CodeSpan s ->
    JObject [("code",   JString s)]

  Link url xs ->
    JObject
      [ ("link", JObject
          [ ("url"    , JString url)
          , ("content", JArray (map (JString . inlineText) xs))
          ])
      ]

  Image url xs ->
    JObject
      [ ("image", JObject
          [ ("url", JString url)
          , ("alt", JArray (map (JString . inlineText) xs))
          ])
      ]

inlineText :: Inline -> String
inlineText = \case
  Plain s      -> s
  Bold xs      -> concatMap inlineText xs
  Italic xs    -> concatMap inlineText xs
  CodeSpan s   -> s
  Link _ xs    -> concatMap inlineText xs
  Image _ xs   -> concatMap inlineText xs

renderValue :: Int -> JValue -> String
renderValue indent = \case
  JArray [] ->
      "[]"

  JString s ->
    "\"" ++ escape s ++ "\""

  JArray xs ->
    "[\n"
    ++ intercalate ",\n"
         [ spaces (indent + 4) ++ renderValue (indent + 4) x
         | x <- xs
         ]
    ++ "\n" ++ spaces indent ++ "]"

  JObject kvs ->
    "{\n"
    ++ intercalate ",\n"
         [ spaces (indent + 4)
           ++ "\"" ++ k ++ "\": "
           ++ renderValue (indent + 4) v
         | (k, v) <- kvs
         ]
    ++ "\n" ++ spaces indent ++ "}"

escape :: String -> String
escape = concatMap esc
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc c    = [c]

spaces :: Int -> String
spaces n = replicate n ' '
