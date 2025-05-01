module Parser.XML
  ( parseXMLDocument,
    parseXMLHeader,
    parseXMLBody,
    parseXMLBlock,
    parseXMLParagraph,
    parseXMLCodeBlock,
    parseXMLList,
    parseXMLListItem,
    parseXMLSection
  ) where

import Parser.Core
    ( Parser,
      satisfy,
      string,
      many,
      choice,
      spaces,
      lexeme,
      parseQuoted )
import Control.Applicative ((<|>))
import AST

-- | Top-level XML Document parser
parseXMLDocument :: Parser Document
parseXMLDocument = lexeme (string "<document>") *> do
  hdr  <- parseXMLHeader
  body <- parseXMLBody
  _ <- lexeme (string "</document>")
  return (Document hdr body)

-- | Parse XML <header title="..."></header>
parseXMLHeader :: Parser Header
parseXMLHeader = lexeme (string "<header") *> do
  _     <- spaces
  _     <- string "title="
  title <- parseQuoted
  _     <- string "></header>"
  return (Header title Nothing Nothing)

-- | Parse XML <body> ... </body>
parseXMLBody :: Parser [Block]
parseXMLBody = lexeme (string "<body>") *> many (lexeme parseXMLBlock) <* lexeme (string "</body>")

-- | Parse any XML block
parseXMLBlock :: Parser Block
parseXMLBlock = choice
  [ parseXMLParagraph
  , parseXMLCodeBlock
  , parseXMLList
  , parseXMLSection
  ]

-- | <paragraph>...</paragraph>
parseXMLParagraph :: Parser Block
parseXMLParagraph = do
  _   <- string "<paragraph>"
  txt <- many (satisfy (/= '<'))
  _   <- string "</paragraph>"
  return (Paragraph [Plain txt])

-- | <codeblock>...</codeblock>
parseXMLCodeBlock :: Parser Block
parseXMLCodeBlock = do
  _    <- string "<codeblock>"
  code <- many (satisfy (/= '<'))
  _    <- string "</codeblock>"
  return (CodeBlock code)

-- | <list type="ordered"> ... </list>
parseXMLList :: Parser Block
parseXMLList = do
  _     <- string "<list"
  _     <- spaces
  _     <- string "type="
  typ   <- parseQuoted
  _     <- string ">"
  items <- many parseXMLListItem
  _     <- string "</list>"
  let lt = if typ == "ordered" then Ordered else Unordered
  return (List lt items)

-- | <item> ... </item>
parseXMLListItem :: Parser [Block]
parseXMLListItem = do
  _      <- string "<item>"
  blocks <- many (lexeme parseXMLBlock)
  _      <- string "</item>"
  return blocks

-- | <section> optional <title> ... </title> then blocks </section>
parseXMLSection :: Parser Block
parseXMLSection = do
  _     <- string "<section>"
  title <- (string "<title>" *> many (satisfy (/= '<')) <* string "</title>") <|> pure []
  blocks<- many (lexeme parseXMLBlock)
  _     <- string "</section>"
  return (Section (if null title then Nothing else Just title) blocks)
