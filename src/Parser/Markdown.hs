{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- main
-}

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

-- Top-level parsers for RawBlock variants
parseRawBlock :: Parser RawBlock
parseRawBlock = choice
  [ parseHeadingBlock
  , parseCodeFenceBlock
  , parseListBlock
  , parseParagraphBlock
  ]

parseHeadingBlock :: Parser RawBlock
parseHeadingBlock = do
  hs <- some (char '#')
  _  <- char ' '
  t  <- manyTill anyChar endOfLine
  return (RHeading (length hs) t)

parseCodeFenceBlock :: Parser RawBlock
parseCodeFenceBlock = do
  _    <- string "```" *> endOfLine
  body <- manyTill (manyTill anyChar endOfLine)
                   (string "```" *> endOfLine)
  return (RCodeBlock body)

parseListBlock :: Parser RawBlock
parseListBlock = do
  items <- some $ do
    _ <- char '-' *> char ' '
    manyTill parseInline endOfLine
  return (RList items)

parseParagraphBlock :: Parser RawBlock
parseParagraphBlock = do
  line <- manyTill anyChar endOfLine
  if all isSpace line
    then fail "blank line"
    else let inls = case runParser inlineParser line of
                      Just (xs, _) -> xs
                      Nothing      -> [Plain line]
         in return (RParagraph inls)

-- 2) BUILD A ROSE TREE BY HEADING LEVEL

data Tree
  = Leaf RawBlock
  | Branch String [Tree]
  deriving Show

-- Entry to build tree
buildTree :: Int -> [RawBlock] -> ([Tree], [RawBlock])
buildTree = collect

-- Collect siblings and leftovers at a given level
collect :: Int -> [RawBlock] -> ([Tree], [RawBlock])
collect _ [] = ([], [])
collect lvl (b:bs) = case b of
  RHeading lev title | lev > lvl ->
    let (node, rest1)     = makeBranch lvl lev title bs
        (siblings, rest2) = collect lvl rest1
    in (node : siblings, rest2)
  RHeading _ _ -> ([], b:bs)
  _            ->
    let (siblings, rest) = collect lvl bs
    in (Leaf b : siblings, rest)

-- Build a single section node and return leftovers
makeBranch :: Int -> Int -> String -> [RawBlock] -> (Tree, [RawBlock])
makeBranch lvl lev title bs =
  let (children, rest) = collect lev bs
      node = if lev == lvl + 1
             then Branch title children
             else Branch "" [Branch title children]
  in (node, rest)

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
