{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module AST where

-- | Document AST for simple Pandoc-like converter
data Document = Document
  { docHeader :: Header      -- ^ Document header
  , docBody   :: [Block]     -- ^ Top-level content blocks
  } deriving (Show, Eq)

-- | Header metadata
data Header = Header
  { headerTitle  :: String        -- ^ Document title
  , headerAuthor :: Maybe String  -- ^ Optional author
  , headerDate   :: Maybe String  -- ^ Optional date
  } deriving (Show, Eq)

-- | Structural blocks in the document
data Block
  = Paragraph [Inline]            -- ^ A paragraph of inline elements
  | Section (Maybe String) [Block]-- ^ A section with optional title and nested blocks
  | CodeBlock String              -- ^ A block of code (raw text)
  | List ListType [[Block]]       -- ^ Ordered or unordered list of items, each item is a list of blocks
  | BlockQuote [Block]            -- ^ A blockquote containing nested blocks
  deriving (Show, Eq)

-- | List ordering
data ListType = Ordered | Unordered
  deriving (Show, Eq)

-- | Inline-level elements within paragraphs or other blocks
data Inline
  = Plain String                  -- ^ Plain text
  | Emphasis [Inline]              -- ^ Italic text
  | Strong [Inline]                -- ^ Bold text
  | CodeSpan String                -- ^ Inline code span
  | Link [Inline] String           -- ^ Hyperlink with link text and URL
  | Image [Inline] String          -- ^ Image with alt text and source URL
  deriving (Show, Eq)
