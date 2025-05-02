{-# LANGUAGE LambdaCase #-}
module Parser.Markdown (parseMarkdownDocument) where

import           Parser.Core
  ( Parser
  , runParser
  , choice
  , spaces
  , string
  , char
  , satisfy
  , many
  , some
  )
import           AST                ( Document(..), Header(..), Block(..), Inline(..) )
import           Control.Applicative ( (<|>), optional )
import           Data.Char          ( isSpace )

parseMarkdownDocument :: Parser Document
parseMarkdownDocument = do
  _ <- spaces
  hdr  <- parseYAMLHeader
  raws <- many (parseRawBlock <* spaces)
  let (forest, _) = buildTree 0 raws
      blocks      = flattenForest forest
  return (Document hdr blocks)

-- 1) RAW AST (block kind + heading level)

data RawBlock
  = RParagraph [Inline]
  | RCodeBlock [String]
  | RList     [[Inline]]
  | RHeading  Int String
  deriving Show

parseRawBlock :: Parser RawBlock
parseRawBlock = choice
  [ parseHeading
  , parseCodeFence
  , parseList
  , parseParagraph
  ]
 where
  -- # Heading
  parseHeading :: Parser RawBlock
  parseHeading = do
    hs <- some (char '#')
    _  <- char ' '
    t  <- manyTill anyChar endOfLine
    return (RHeading (length hs) t)

  -- ``` code fence ```
  parseCodeFence :: Parser RawBlock
  parseCodeFence = do
    _    <- string "```" *> endOfLine
    body <- manyTill (manyTill anyChar endOfLine)
                     (string "```" *> endOfLine)
    return (RCodeBlock body)

  -- - list items (one line each)
  parseList :: Parser RawBlock
  parseList = do
    items <- some $ do
      _   <- char '-' *> char ' '
      inl <- manyTill parseInline endOfLine
      return inl
    return (RList items)

  -- paragraph = single non-empty line
  parseParagraph :: Parser RawBlock
  parseParagraph = do
    line <- manyTill anyChar endOfLine
    if all isSpace line
      then fail "blank line"
      else
        let inls = case runParser inlineParser line of
                     Just (xs, _) -> xs
                     Nothing      -> [Plain line]
        in return (RParagraph inls)

-- 2) BUILD A ROSE TREE BY HEADING LEVEL

data Tree
  = Leaf RawBlock
  | Branch String [Tree]
  deriving Show

-- buildTree lvl rawBlocks = (forest, leftovers)
buildTree :: Int -> [RawBlock] -> ([Tree], [RawBlock])
buildTree _ [] = ([], [])
buildTree lvl bs@(b:bs') = case b of

  RHeading lev title
    | lev > lvl ->
      -- collect children under this heading
      let (children, rest1)  = buildTree lev bs'
          sectionNode        = Branch title children
          -- if skipping levels, wrap in anonymous section
          branchNode
            | lev == lvl + 1 = sectionNode
            | otherwise      = Branch "" [sectionNode]
          (siblings, rest2)  = buildTree lvl rest1
      in (branchNode : siblings, rest2)

    | otherwise ->
      -- this heading belongs to an outer level: stop here
      ([], bs)

  _ ->
    -- non-heading => leaf, keep going
    let (siblings, rest) = buildTree lvl bs'
    in (Leaf b : siblings, rest)

-- 3) FLATTEN THE TREE INTO REAL AST.Block

flattenForest :: [Tree] -> [Block]
flattenForest = concatMap go where
  go (Leaf (RParagraph inl)) = [ Paragraph inl    ]
  go (Leaf (RCodeBlock ls))  = [ CodeBlock ls      ]
  go (Leaf (RList items))    = [ List items        ]
  go (Leaf  _               ) = []
  go (Branch title subs)     = [ Section title (flattenForest subs) ]

-- 4) INLINE PARSER

inlineParser :: Parser [Inline]
inlineParser = many parseInline

parseInline :: Parser Inline
parseInline = choice
  [ parseImage
  , parseLink
  , parseBold
  , parseItalic
  , parseCodeSpan
  , parsePlain
  ]

parsePlain :: Parser Inline
parsePlain = Plain <$> some (satisfy (`notElem` startChars))
  where startChars = "*_`![\n"

parseBold :: Parser Inline
parseBold = do
  _  <- string "**"
  xs <- manyTill parseInline (string "**")
  return (Bold xs)

parseItalic :: Parser Inline
parseItalic = do
  _  <- char '*'
  xs <- manyTill parseInline (char '*')
  return (Italic xs)

parseCodeSpan :: Parser Inline
parseCodeSpan = do
  _ <- char '`'
  s <- many (satisfy (/= '`'))
  _ <- char '`'
  return (CodeSpan s)

parseLink :: Parser Inline
parseLink = do
  _   <- char '['
  txt <- many (satisfy (/= ']'))
  _   <- string "]("
  url <- many (satisfy (/= ')'))
  _   <- char ')'
  return (Link url [Plain txt])

parseImage :: Parser Inline
parseImage = do
  _   <- string "!["
  alt <- many (satisfy (/= ']'))
  _   <- string "]("
  url <- many (satisfy (/= ')'))
  _   <- char ')'
  return (Image url [Plain alt])

-- 5) YAML FRONT-MATTER & UTILITIES

parseYAMLHeader :: Parser Header
parseYAMLHeader = do
  _    <- string "---" *> endOfLine
  ttl  <- string "title:"  *> spaces *> manyTill anyChar endOfLine
  auth <- optional (string "author:" *> spaces *> manyTill anyChar endOfLine)
  dt   <- optional (string "date:"   *> spaces *> manyTill anyChar endOfLine)
  _    <- string "---" *> endOfLine
  _    <- many endOfLine
  let stripBoth = strip
  return (Header (stripBoth ttl) (stripBoth <$> auth) (stripBoth <$> dt))

anyChar   :: Parser Char
anyChar   = satisfy (const True)

endOfLine :: Parser Char
endOfLine = char '\n'

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = go where
  go = (end >> pure []) <|> ((:) <$> p <*> go)

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace
  where dropWhileEnd p = reverse . dropWhile p . reverse
