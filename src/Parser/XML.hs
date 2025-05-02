{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- main
-}

module Parser.XML
  ( parseXMLDocument,
    parseXMLHeader,
    parseXMLBody,
    parseXMLBlock,
    parseXMLSection,
    parseXMLCodeBlock,
    parseXMLList,
    parseXMLParagraph,
    parseInline,
  ) where

import Parser.Core
import AST
import Control.Applicative (optional)
import Prelude hiding (lines)

-- | Top-level XML Document parser
parseXMLDocument :: Parser Document
parseXMLDocument = spaces *> lexeme (string "<document>") *> do
  hdr  <- parseXMLHeader
  body <- parseXMLBody
  _ <- lexeme (string "</document>")
  return (Document hdr body)

-- | Parse XML <header> with attributes and optional author/date children
parseXMLHeader :: Parser Header
parseXMLHeader = lexeme (string "<header") *> do
  _     <- spaces
  title <- attribute "title"
  _     <- lexeme (string ">")
  auth  <- optional (spaces *> string "<author>" *> many (satisfy (/= '<')) <* string "</author>")
  date  <- optional (spaces *> string "<date>"   *> many (satisfy (/= '<')) <* string "</date>")
  _ <- lexeme (string "</header>")
  return (Header title auth date)
  where
    attribute name = string (name ++ "=\"") *> many (satisfy (/= '"')) <* char '"'

-- | Parse XML <body> ... </body>
parseXMLBody :: Parser [Block]
parseXMLBody = lexeme (string "<body>") *>
    many parseXMLBlock <* lexeme (string "</body>")

-- | Parse any block inside body
parseXMLBlock :: Parser Block
parseXMLBlock = lexeme $ choice
  [ parseXMLSection
  , parseXMLCodeBlock
  , parseXMLList
  , parseXMLParagraph
  ]

-- | <paragraph>content</paragraph>
parseXMLParagraph :: Parser Block
parseXMLParagraph = do
  _   <- lexeme (string "<paragraph>")
  inls <- many parseInline
  _   <- lexeme (string "</paragraph>")
  return (Paragraph inls)

-- | Inline elements: bold, italic, code, link, image or plain text
parseInline :: Parser Inline
parseInline = choice
  [ parseBold
  , parseItalic
  , parseCodeSpan
  , parseLink
  , parseImage
  , parsePlain
  ]

parsePlain :: Parser Inline
parsePlain = Plain <$> some (satisfy (/= '<'))

parseBold :: Parser Inline
parseBold = do
  _ <- string "<bold>"
  xs <- many parseInline
  _ <- string "</bold>"
  return (Bold xs)

parseItalic :: Parser Inline
parseItalic = do
  _ <- string "<italic>"
  xs <- many parseInline
  _ <- string "</italic>"
  return (Italic xs)

parseCodeSpan :: Parser Inline
parseCodeSpan = do
  _ <- string "<code>"
  s <- many (satisfy (/= '<'))
  _ <- string "</code>"
  return (CodeSpan s)

parseLink :: Parser Inline
parseLink = do
  _   <- string "<link url=\""
  url <- many (satisfy (/= '"'))
  _   <- string "\">"
  xs  <- many parseInline
  _   <- string "</link>"
  return (Link url xs)

parseImage :: Parser Inline
parseImage = do
  _   <- string "<image url=\""
  url <- many (satisfy (/= '"'))
  _   <- string "\">"
  xs  <- many parseInline
  _   <- string "</image>"
  return (Image url xs)

-- | <section title="..."> ... </section>
parseXMLSection :: Parser Block
parseXMLSection = do
  _   <- lexeme (string "<section")
  _   <- spaces
  title <- optional (string "title=\"" *> many (satisfy (/= '"')) <* char '"')
  _   <- lexeme (string ">")
  bs  <- many parseXMLBlock
  _   <- lexeme (string "</section>")
  return (Section (maybe "" id title) bs)

-- | <codeblock> containing paragraphs
parseXMLCodeBlock :: Parser Block
parseXMLCodeBlock = do
  _   <- lexeme (string "<codeblock>")
  ps <- many (lexeme parseXMLParagraph)
  _   <- lexeme (string "</codeblock>")
  let lines = [concatMap inlineText inls | Paragraph inls <- ps]
  return (CodeBlock lines)

-- | <list> of paragraph items
parseXMLList :: Parser Block
parseXMLList = do
  _   <- lexeme (string "<list")
  _   <- optional (spaces *> string "type=\"" *>
    many (satisfy (/= '"')) <* char '"')
  _   <- lexeme (string ">")
  ps  <- many parseXMLParagraph
  _   <- lexeme (string "</list>")
  let items = [inls | Paragraph inls <- ps]
  return (List items)

-- helper to extract text from Inline
inlineText :: Inline -> String
inlineText (Plain s)       = s
inlineText (Bold xs)       = concatMap inlineText xs
inlineText (Italic xs)     = concatMap inlineText xs
inlineText (CodeSpan s)    = s
inlineText (Link _ xs)     = concatMap inlineText xs
inlineText (Image _ xs)    = concatMap inlineText xs
