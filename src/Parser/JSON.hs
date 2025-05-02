{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- main
-}

{-# LANGUAGE LambdaCase #-}
module Parser.JSON (
  parseJSONDocument
  ) where

import Parser.Core (Parser, lexeme, char, parseQuoted)
import AST                  ( Document(Document)
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

-- | Convert the top-level JValue into our Document AST
convertDoc :: JValue -> Parser Document
convertDoc = \case
  JObject kv -> do
    -- header
    hProps <- case lookup "header" kv of
                Just (JObject hp) -> return hp
                _                 -> empty

    title <- case lookup "title" hProps of
               Just (JString s) -> return s
               _                -> empty

    let auth = case lookup "author" hProps of
                 Just (JString s) -> Just s
                 _                -> Nothing
        date = case lookup "date" hProps of
                 Just (JString s) -> Just s
                 _                -> Nothing

    -- body
    bodyVals <- case lookup "body" kv of
                  Just (JArray arr) -> return arr
                  _                 -> empty
    blocks <- mapM convertBlock bodyVals

    return (Document (Header title auth date) blocks)

  _ -> empty

-- | Convert a JSON value to a Block
convertBlock :: JValue -> Parser Block
convertBlock = \case
  -- Paragraph: an array of inlines
  JArray inls ->
    Paragraph <$> mapM convertInline inls

  -- Section
  JObject [("section", JObject sp)] -> do
    title <- case lookup "title" sp of
               Just (JString s) -> return s
               _                -> empty
    contentArr <- case lookup "content" sp of
                    Just (JArray bs) -> return bs
                    _                -> empty
    blocks <- mapM convertBlock contentArr
    return (Section title blocks)

  -- CodeBlock
  JObject [("codeblock", JArray cs)] -> do
    codes <- for cs $ \case
      JString s -> return s
      _         -> empty
    return (CodeBlock codes)

  -- List
  JObject [("list", JArray items)] -> do
    lists <- for items $ \case
      JArray inls -> mapM convertInline inls
      _           -> empty
    return (List lists)

  _ -> empty

-- | Convert a JSON value to an Inline
convertInline :: JValue -> Parser Inline
convertInline = \case
  JString s ->
    return (Plain s)

  JObject [("bold", JString s)] ->
    return (Bold [Plain s])

  JObject [("bold", JArray xs)] -> do
    inls <- mapM convertInline xs
    return (Bold inls)

  JObject [("italic", JString s)] ->
    return (Italic [Plain s])
    
  JObject [("italic", JArray xs)] -> do
    inls <- mapM convertInline xs
    return (Italic inls)

  JObject [("code", JString s)] ->
    return (CodeSpan s)

  -- Link
  JObject [("link", JObject lp)] -> do
    url <- case lookup "url" lp of
             Just (JString u) -> return u
             _                -> empty
    contArr <- case lookup "content" lp of
                 Just (JArray cs) -> return cs
                 _                -> empty
    inls <- for contArr $ \case
      JString t -> return (Plain t)
      _         -> empty
    return (Link url inls)

  -- Image
  JObject [("image", JObject ip)] -> do
    url <- case lookup "url" ip of
             Just (JString u) -> return u
             _                -> empty
    altArr <- case lookup "alt" ip of
                Just (JArray as) -> return as
                _                -> empty
    inls <- for altArr $ \case
      JString t -> return (Plain t)
      _         -> empty
    return (Image url inls)

  _ -> empty

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
