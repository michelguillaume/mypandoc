{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- JSON parser
-}

module Parser.JSON (
  parseJSONDocument
  ) where

import Parser.Core (Parser, lexeme, char, parseQuoted)
import AST                ( Document(Document)
                           , Header(Header)
                           , Block(..)
                           , Inline(..)
                           )
import Control.Applicative  (Alternative(empty), (<|>), many)

-- A tiny JSON AST
data JValue
  = JObject [(String, JValue)]
  | JArray  [JValue]
  | JString String
  deriving Show

-- | Top-level parser: read a JValue then convert it to Document
parseJSONDocument :: Parser Document
parseJSONDocument = lexeme parseJValue >>= convertDoc

-- | Parse any JSON value (object, array or string)
parseJValue :: Parser JValue
parseJValue = lexeme $
      parseObject
  <|> parseArray
  <|> (JString <$> parseQuoted)

parseObject :: Parser JValue
parseObject = do
  _     <- lexeme (char '{')
  pairs <- sepBy parsePair (lexeme (char ','))
  _     <- lexeme (char '}')
  return (JObject pairs)

parsePair :: Parser (String, JValue)
parsePair = do
  key <- parseQuoted
  _   <- lexeme (char ':')
  val <- parseJValue
  return (key, val)

parseArray :: Parser JValue
parseArray = do
  _   <- lexeme (char '[')
  vs  <- sepBy parseJValue (lexeme (char ','))
  _   <- lexeme (char ']')
  return (JArray vs)

-- | Require a nested JSON object by key
requireObject :: String -> [(String, JValue)] -> Parser [(String, JValue)]
requireObject key kv = case lookup key kv of
  Just (JObject o) -> return o
  _                -> empty

-- | Require a JSON string by key
requireString :: String -> [(String, JValue)] -> Parser String
requireString key kv = case lookup key kv of
  Just (JString s) -> return s
  _                -> empty

-- | Optional JSON string by key
optionalString :: String -> [(String, JValue)] -> Parser (Maybe String)
optionalString key kv = case lookup key kv of
  Just (JString s) -> return (Just s)
  _                -> return Nothing

-- | Extract header from JSON key-value list (<=10 lines)
parseJSONHeader :: [(String, JValue)] -> Parser Header
parseJSONHeader kv = do
  hp    <- requireObject "header" kv
  title <- requireString "title" hp
  auth  <- optionalString "author" hp
  date  <- optionalString "date" hp
  return (Header title auth date)

-- | Extract body blocks from JSON key-value list
parseJSONBody :: [(String, JValue)] -> Parser [Block]
parseJSONBody kv = do
  arr <- case lookup "body" kv of
           Just (JArray xs) -> return xs
           _                -> empty
  mapM convertBlock arr

-- | Convert a JSON value to a Block
convertBlock :: JValue -> Parser Block
convertBlock val = case val of
  JArray inls                        -> parseParagraphBlockJson inls
  JObject [("section", JObject sp)] -> parseSectionBlockJson sp
  JObject [("codeblock", JArray cs)] -> parseCodeBlockJson cs
  JObject [("list", JArray items)]   -> parseListBlockJson items
  _                                   -> empty

-- Paragraph block from a JSON array
parseParagraphBlockJson :: [JValue] -> Parser Block
parseParagraphBlockJson inls =
  Paragraph <$> mapM convertInline inls

-- Section block from a JSON object
parseSectionBlockJson :: [(String, JValue)] -> Parser Block
parseSectionBlockJson sp = do
  title <- requireString "title" sp
  contentArr <- case lookup "content" sp of
                  Just (JArray bs) -> return bs
                  _                -> empty
  blocks <- mapM convertBlock contentArr
  return (Section title blocks)

-- Code block from a JSON array
parseCodeBlockJson :: [JValue] -> Parser Block
parseCodeBlockJson cs = do
  codes <- for cs (\v -> case v of
    JString s -> return s
    _         -> empty)
  return (CodeBlock codes)

-- List block from a JSON array
parseListBlockJson :: [JValue] -> Parser Block
parseListBlockJson items = do
  lists <- for items (\v -> case v of
    JArray inls -> mapM convertInline inls
    _           -> empty)
  return (List lists)

-- | Convert the top-level JValue into a Document AST
convertDoc :: JValue -> Parser Document
convertDoc val = case val of
  JObject kv -> do
    hdr    <- parseJSONHeader kv
    blocks <- parseJSONBody kv
    return (Document hdr blocks)
  _ -> empty

-- | Parser helpers for Inline conversion
parsePlainInline :: String -> Parser Inline
parsePlainInline s = return (Plain s)

parseBoldInline :: [JValue] -> Parser Inline
parseBoldInline xs = do
  inls <- mapM convertInline xs
  return (Bold inls)

parseItalicInline :: [JValue] -> Parser Inline
parseItalicInline xs = do
  inls <- mapM convertInline xs
  return (Italic inls)

parseLinkInline :: [(String, JValue)] -> Parser Inline
parseLinkInline lp = do
  url <- requireString "url" lp
  arr <- case lookup "content" lp of
           Just (JArray xs) -> return xs
           _                -> empty
  inls <- mapM convertInline arr
  return (Link url inls)

parseImageInline :: [(String, JValue)] -> Parser Inline
parseImageInline ip = do
  url <- requireString "url" ip
  arr <- case lookup "alt" ip of
           Just (JArray xs) -> return xs
           _                -> empty
  inls <- mapM convertInline arr
  return (Image url inls)

-- | Convert a JSON value to an Inline
convertInline :: JValue -> Parser Inline
convertInline val = case val of
  JString s                        -> parsePlainInline s
  JObject [("bold", JString s)]  -> return (Bold [Plain s])
  JObject [("bold", JArray xs)]   -> parseBoldInline xs
  JObject [("italic", JString s)]-> return (Italic [Plain s])
  JObject [("italic", JArray xs)]-> parseItalicInline xs
  JObject [("code", JString s)]  -> return (CodeSpan s)
  JObject [("link", JObject lp) ]-> parseLinkInline lp
  JObject [("image", JObject ip)]-> parseImageInline ip
  _                                -> empty

-- | Zero-or-more list separated by sep
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep =
      (:) <$> p <*> many (sep *> p)
  <|> pure []

-- | Helper: like mapM but for pure lists
for :: [a] -> (a -> Parser b) -> Parser [b]
for []     _ = return []
for (x:xs) f = do
  y  <- f x
  ys <- for xs f
  return (y:ys)
