{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- JSON renderer
-}

module Format.JSON (renderJSON) where

import AST
import Data.List (intercalate)

-- JSON AST
data JValue
  = JObject [(String, JValue)]
  | JArray  [JValue]
  | JString String

-- | Render a Document to a JSON string
renderJSON :: Document -> String
renderJSON doc =
  renderValue 0 (JObject
    [ ("header", headerValue (docHeader doc))
    , ("body"  , JArray  (map blockValue (docBody doc)))
    ])

-- | Convert Header to JValue
headerValue :: Header -> JValue
headerValue (Header title mAuth mDate) = JObject
  ([ ("title", JString title) ]
  ++ maybe [] (\a -> [("author", JString a)]) mAuth
  ++ maybe [] (\d -> [("date"  , JString d)]) mDate
  )

-- | Convert Block to JValue
type BlockValue = Block -> JValue
blockValue :: BlockValue
blockValue (Paragraph inls)  = paragraphValue inls
blockValue (CodeBlock cs)    = codeBlockValue cs
blockValue (List items)      = listValue items
blockValue (Section t bs)    = sectionValue t bs

-- Paragraph to JValue
paragraphValue :: [Inline] -> JValue
paragraphValue inls = JArray (map inlineValue inls)

-- CodeBlock to JValue
codeBlockValue :: [String] -> JValue
codeBlockValue cs = JObject [("codeblock", JArray (map JString cs))]

-- List to JValue
listValue :: [[Inline]] -> JValue
listValue items = JObject [("list", JArray
  [ JArray (map (JString . inlineText) ins)
  | ins <- items
  ])]

-- Section to JValue
sectionValue :: String -> [Block] -> JValue
sectionValue t bs = JObject [("section", JObject
  [ ("title",   JString t)
  , ("content", JArray (map blockValue bs))
  ])]

-- | Convert Inline to JValue
inlineValue :: Inline -> JValue
inlineValue (Plain s)     = JString s
inlineValue (Bold xs)
  | all isPlain xs = JObject [("bold", JString (concatMap inlineText xs))]
  | otherwise = JObject [("bold", JArray  (map inlineValue xs))]
inlineValue (Italic xs)
  | all isPlain xs = JObject [("italic", JString (concatMap inlineText xs))]
  | otherwise = JObject [("italic", JArray  (map inlineValue xs))]
inlineValue (CodeSpan s)  = JObject [("code",   JString s)]
inlineValue (Link u xs)   = JObject [("link", JObject
  [ ("url",     JString u)
  , ("content", JArray (map (JString . inlineText) xs))
  ])]
inlineValue (Image u xs)  = JObject [("image", JObject
  [ ("url", JString u)
  , ("alt", JArray (map (JString . inlineText) xs))
  ])]

-- | Check if Inline is plain text only
isPlain :: Inline -> Bool
isPlain (Plain _) = True
isPlain _         = False

-- | Extract text from Inline
inlineText :: Inline -> String
inlineText (Plain s)    = s
inlineText (Bold xs)    = concatMap inlineText xs
inlineText (Italic xs)  = concatMap inlineText xs
inlineText (CodeSpan s) = s
inlineText (Link _ xs)  = concatMap inlineText xs
inlineText (Image _ xs) = concatMap inlineText xs

-- | Render JValue with indentation
renderValue :: Int -> JValue -> String
renderValue ind val = case val of
  JArray []    -> renderEmptyArray
  JString s    -> renderString s
  JArray xs    -> renderArray ind xs
  JObject kvs  -> renderObject ind kvs

-- | Empty JSON array
renderEmptyArray :: String
renderEmptyArray = "[]"

-- | JSON string literal
renderString :: String -> String
renderString s = "\"" ++ escape s ++ "\""

-- | Render JSON array with indentation
renderArray :: Int -> [JValue] -> String
renderArray ind xs = "[\n"
  ++ intercalate ",\n"
       [ spaces (ind+4) ++ renderValue (ind+4) x | x <- xs ]
  ++ "\n" ++ spaces ind ++ "]"

-- | Render JSON object with indentation
renderObject :: Int -> [(String,JValue)] -> String
renderObject ind kvs = "{\n"
  ++ intercalate ",\n"
       [ spaces (ind+4) ++ "\"" ++ k ++ "\": " ++ renderValue (ind+4) v
       | (k,v) <- kvs ]
  ++ "\n" ++ spaces ind ++ "}"

-- | Escape JSON strings
escape :: String -> String
escape = concatMap esc
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc c     = [c]

-- | Spaces for indentation
spaces :: Int -> String
spaces n = replicate n ' '
