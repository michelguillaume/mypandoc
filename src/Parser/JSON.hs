module Parser.JSON
  ( parseJSONDocument,
    parseJSONArrayBody,
    parseJSONStringBlock,
  ) where

import Parser.Core
import AST

-- | Top-level JSON Document parser (very minimal)
parseJSONDocument :: Parser Document
parseJSONDocument = lexeme (char '{') *> do
  _      <- lexeme (string "\"header\"")
  _      <- lexeme (char ':')
  _      <- lexeme (char '{')
  _      <- lexeme (string "\"title\"")
  _      <- lexeme (char ':')
  title  <- parseQuoted
  _      <- lexeme (char '}')
  _      <- lexeme (char ',')
  _      <- lexeme (string "\"body\"")
  _      <- lexeme (char ':')
  blocks <- parseJSONArrayBody
  _      <- lexeme (char '}')
  return (Document (Header title Nothing Nothing) blocks)

-- | Parse JSON array of strings (simple paragraphs)
parseJSONArrayBody :: Parser [Block]
parseJSONArrayBody = lexeme (char '[') *> many (lexeme parseJSONStringBlock) <* lexeme (char ']')

-- | Parse a quoted string as a paragraph block
parseJSONStringBlock :: Parser Block
parseJSONStringBlock = do
  str <- parseQuoted
  return (Paragraph [Plain str])
