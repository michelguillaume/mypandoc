{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- Markdown parser
-}

module Parser.Markdown (parseMarkdownDocument) where

import Parser.Core
  ( Parser
  , choice
  , spaces
  , string
  , char
  , satisfy
  , many
  , some
  , runParser
  )
import AST
  ( Document(..)
  , Header(..)
  , Block(..)
  , Inline(..)
  )
import Control.Applicative ((<|>), optional)
import Data.Char           (isSpace)
import Data.List           (isPrefixOf)

-- Entry point
parseMarkdownDocument :: Parser Document
parseMarkdownDocument = do
  _    <- spaces
  hdr  <- parseYAMLHeader
  raws <- many (parseRawBlock <* spaces)
  let (forest, _) = buildTree 0 raws
  pure $ Document hdr (flattenForest forest)

-- RawBlock representation

data RawBlock
  = RParagraph [Inline]
  | RList     [[RawBlock]]
  | RHeading  Int String
  | RCodeBlock [RawBlock]
  deriving Show

-- Parse any block
parseRawBlock :: Parser RawBlock
parseRawBlock = choice
  [ parseHeading
  , parseCodeFence
  , parseList
  , parseParagraph
  ]

-- Heading
parseHeading :: Parser RawBlock
parseHeading = do
  hashes <- some (char '#')
  _      <- char ' '
  title  <- manyTill anyChar endOfLine
  pure $ RHeading (length hashes) title

-- Code fences
parseCodeFence :: Parser RawBlock
parseCodeFence = parseMultilineFence <|> parseInlineFence

parseInlineFence :: Parser RawBlock
parseInlineFence = do
  _       <- string "```"
  content <- many (satisfy (/= '`'))
  _       <- string "```" *> endOfLine
  let para = RParagraph [Plain content]
  pure $ RCodeBlock [para]

parseMultilineFence :: Parser RawBlock
parseMultilineFence = do
  _      <- string "```" *> endOfLine
  blocks <- manyTill fenceItem (string "```" *> endOfLine)
  pure $ RCodeBlock blocks

fenceItem :: Parser RawBlock
fenceItem = parseInlineFence
         <|> (RParagraph . pure . Plain <$> manyTill anyChar endOfLine)

-- List
parseList :: Parser RawBlock
parseList = RList <$> some parseListEntry

parseListEntry :: Parser [RawBlock]
parseListEntry = do
  _     <- string "- "
  block <- parseMultilineFence
        <|> parseInlineFence
        <|> (RParagraph <$> manyTill parseInline endOfLine)
  pure [block]

-- Paragraph
parseParagraph :: Parser RawBlock
parseParagraph = do
  line <- manyTill anyChar endOfLine
  if all isSpace line || any (`isPrefixOf` line) ["```", "- "]
    then fail "split"
    else let txt  = strip line
             inls = case runParser inlineParser txt of
                      Just (xs, "") -> xs
                      _               -> [Plain txt]
         in pure $ RParagraph inls

invalidLine :: String -> Bool
invalidLine ln = all isSpace ln
              || "```" `isPrefixOf` ln
              || "- " `isPrefixOf` ln

-- Section tree

data Tree = Leaf RawBlock | Branch String [Tree]
  deriving Show

buildTree :: Int -> [RawBlock] -> ([Tree], [RawBlock])
buildTree = collect

collect :: Int -> [RawBlock] -> ([Tree], [RawBlock])
collect _ [] = ([], [])
collect lvl (RHeading lev title : bs)
  | lev > lvl =
      let (ch, r1) = collect lev bs
          node     = if lev == lvl + 1
                     then Branch title ch
                     else Branch "" [Branch title ch]
          (sib, r2) = collect lvl r1
      in (node : sib, r2)
collect lvl (b:bs) =
  let (sib, rest) = collect lvl bs
  in (Leaf b : sib, rest)

-- Flatten to Blocks
flattenForest :: [Tree] -> [Block]
flattenForest = concatMap treeToBlocks

treeToBlocks :: Tree -> [Block]
treeToBlocks (Leaf rb)     = [rawToBlock rb]
treeToBlocks (Branch t cs) = [Section t (flattenForest cs)]

-- RawBlock -> Block
rawToBlock :: RawBlock -> Block
rawToBlock (RParagraph xs)      = Paragraph xs
rawToBlock (RList items)        = List (map (map rawToBlock) items)
rawToBlock (RCodeBlock rbs)     =
  CodeBlock (concatMap toInnerBlocks rbs)
rawToBlock _ = error "Unexpected block"

toInnerBlocks :: RawBlock -> [Block]
toInnerBlocks (RCodeBlock i) = [CodeBlock (concatMap toInnerBlocks i)]
toInnerBlocks rb             = [rawToBlockSimple rb]

rawToBlockSimple :: RawBlock -> Block
rawToBlockSimple (RParagraph xs) = Paragraph xs
rawToBlockSimple (RList items)   = List (map (map rawToBlockSimple) items)
rawToBlockSimple _               = error "Unexpected raw block"

-- Inline parsing
inlineParser :: Parser [Inline]
inlineParser = many parseInline

parseInline :: Parser Inline
parseInline = choice
  [ parseImage
  , parseLink
  , parseTriple
  , parseBold
  , parseItalic
  , parseCodeSpan
  , parsePlain
  ]

parseTriple :: Parser Inline
parseTriple = do
  _  <- string "***"
  xs <- manyTill parseInline (string "***")
  pure $ Italic [Bold xs]

parsePlain :: Parser Inline
parsePlain = Plain <$> some (satisfy validPlain)
  where
    validPlain c = c /= '\n' && c `notElem` "*_`!["

parseBold :: Parser Inline
parseBold = do
  _  <- string "**"
  xs <- manyTill parseInline (string "**")
  pure $ Bold xs

parseItalic :: Parser Inline
parseItalic = do
  _  <- char '*'
  xs <- manyTill parseInline (char '*')
  pure $ Italic xs

parseCodeSpan :: Parser Inline
parseCodeSpan = do
  _ <- char '`'
  s <- many (satisfy (/= '`'))
  _ <- char '`'
  pure $ CodeSpan s

parseLink :: Parser Inline
parseLink = do
  _   <- char '['
  txt <- many (satisfy (/= ']'))
  _   <- string "]("
  url <- many (satisfy (/= ')'))
  _   <- char ')'
  let raw = maybe [Plain txt] fst (runParser inlineParser txt)
  pure $ makeLink url raw

makeLink :: String -> [Inline] -> Inline
makeLink url inls = Link url (filter valid inls)
  where
    valid (Plain s) = not (all isSpace s)
    valid _         = True

parseImage :: Parser Inline
parseImage = do
  _   <- string "!["
  alt <- many (satisfy (/= ']'))
  _   <- string "]("
  url <- many (satisfy (/= ')'))
  _   <- char ')'
  let inls = case runParser inlineParser alt of
               Just (xs, _) -> xs
               Nothing      -> [Plain alt]
  pure $ Image url inls

-- YAML header
parseYAMLHeader :: Parser Header
parseYAMLHeader = do
  _     <- string "---" *> endOfLine
  title <- headerField "title:"
  auth  <- optional (headerField "author:")
  dt    <- optional (headerField "date:")
  _     <- string "---" *> endOfLine *> many endOfLine
  pure $ Header (strip title) (strip <$> auth) (strip <$> dt)

headerField :: String -> Parser String
headerField key = string key *> spaces *> manyTill anyChar endOfLine

anyChar :: Parser Char
anyChar = satisfy (const True)

endOfLine :: Parser Char
endOfLine = char '\n'

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = go where
  go = (end *> pure []) <|> ((:) <$> p <*> go)

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace
  where dropWhileEnd p = reverse . dropWhile p . reverse