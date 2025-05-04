{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- AST data structure
-}

module AST where

data Document = Document
  { docHeader :: Header
  , docBody   :: [Block]
  } deriving (Show, Eq)

data Header = Header
  { headerTitle  :: String
  , headerAuthor :: Maybe String
  , headerDate   :: Maybe String
  } deriving (Show, Eq)


data Block
  = Paragraph [Inline]
  | Section   String [Block]
  | CodeBlock [Block]
  | List      [[Block]]
  | Raw       String
  deriving (Show, Eq)


data Inline
  = Plain    String
  | Bold     [Inline]
  | Italic   [Inline]
  | CodeSpan String
  | Link     String [Inline]
  | Image    String [Inline]
  deriving (Show, Eq)
