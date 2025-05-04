{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- XML parser
-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Parser.XML (parseXMLDocument) where

import Parser.Core       ( Parser, lexeme, char, string, spaces
                         , satisfy, many, some, choice )
import AST               ( Document(..), Header(..), Block(..), Inline(..) )
import Control.Applicative ( optional)
import Data.Maybe         ( fromMaybe )

-- | Parse a full <document>…</document>
parseXMLDocument :: Parser Document
parseXMLDocument = spaces *> lexeme (string "<document>") *> do
  hdr <- parseXMLHeader
  bs  <- parseXMLBody
  _   <- lexeme (string "</document>")
  return (Document hdr bs)

parseHeaderOpen :: Parser String
parseHeaderOpen =
  lexeme (string "<header")
  *> spaces
  *> attribute "title"
  <* lexeme (string ">")

parseHeaderAuthor :: Parser (Maybe String)
parseHeaderAuthor = optional (spaces *> xmlTag "author")

parseHeaderDate :: Parser (Maybe String)
parseHeaderDate = optional (spaces *> xmlTag "date")

parseHeaderClose :: Parser ()
parseHeaderClose = lexeme (string "</header>") *> pure ()

parseXMLHeader :: Parser Header
parseXMLHeader =
  Header
    <$> parseHeaderOpen
    <*> parseHeaderAuthor
    <*> parseHeaderDate
    <*  parseHeaderClose

xmlTag :: String -> Parser String
xmlTag name =
  lexeme (string ("<" ++ name ++ ">"))
  *> many (satisfy (/= '<'))
  <* lexeme (string ("</" ++ name ++ ">"))

parseXMLBody :: Parser [Block]
parseXMLBody =
  lexeme (string "<body>")
  *> many parseXMLBlock
  <* lexeme (string "</body>")

parseXMLBlock :: Parser Block
parseXMLBlock = lexeme $ choice
  [ parseXMLSection
  , parseXMLCodeBlock
  , parseXMLList
  , parseXMLParagraph
  , parseXMLRaw
  ]

parseXMLParagraph :: Parser Block
parseXMLParagraph = do
  _  <- lexeme (string "<paragraph>")
  xs <- many parseXMLInline
  _  <- lexeme (string "</paragraph>")
  return (Paragraph xs)

parseXMLCodeBlock :: Parser Block
parseXMLCodeBlock = do
  _  <- lexeme (string "<codeblock>")
  bs <- many parseXMLBlock
  _  <- lexeme (string "</codeblock>")
  return (CodeBlock bs)

parseXMLList :: Parser Block
parseXMLList =
  do
    _  <- lexeme (string "<list")
    _  <- optional (spaces *> string "type=\""
      *> many (satisfy (/= '\"')) <* char '\"')
    _  <- lexeme (string ">")
    bs <- many parseXMLBlock
    _  <- lexeme (string "</list>")
    let items = [ [b] | b <- bs ]
    return (List items)


parseXMLRaw :: Parser Block
parseXMLRaw = do
  s <- many1 (satisfy (/= '<'))
  return (Raw s)
  where
    many1 p = (:) <$> p <*> many p

parseXMLSection :: Parser Block
parseXMLSection = do
  _     <- lexeme (string "<section")
  _     <- spaces
  t     <- optional (string "title=\"" *> many (satisfy (/= '"')) <* char '"')
  _     <- lexeme (string ">")
  bs    <- many parseXMLBlock
  _     <- lexeme (string "</section>")
  return (Section (fromMaybe "" t) bs)

parseXMLInline :: Parser Inline
parseXMLInline = choice
  [ parseBold
  , parseItalic
  , parseCodeSpanInline
  , parseLink
  , parseImage
  , parsePlain
  ]

parsePlain :: Parser Inline
parsePlain = Plain <$> some (satisfy (/= '<'))

parseBold :: Parser Inline
parseBold = do
  _  <- string "<bold>"
  xs <- many parseXMLInline
  _  <- string "</bold>"
  return (Bold xs)

parseItalic :: Parser Inline
parseItalic = do
  _  <- string "<italic>"
  xs <- many parseXMLInline
  _  <- string "</italic>"
  return (Italic xs)

parseCodeSpanInline :: Parser Inline
parseCodeSpanInline = do
  _ <- string "<code>"
  s <- many (satisfy (/= '<'))
  _ <- string "</code>"
  return (CodeSpan s)

parseLink :: Parser Inline
parseLink = do
  _   <- string "<link url=\""
  u   <- many (satisfy (/= '"'))
  _   <- string "\">"
  xs  <- many parseXMLInline
  _   <- string "</link>"
  return (Link u xs)

parseImage :: Parser Inline
parseImage = do
  _   <- string "<image url=\""
  u   <- many (satisfy (/= '"'))
  _   <- string "\">"
  xs  <- many parseXMLInline
  _   <- string "</image>"
  return (Image u xs)

-- | Attribute parser: name="..."
attribute :: String -> Parser String
attribute name = string (name ++ "=\"") *> many (satisfy (/= '"')) <* char '"'
